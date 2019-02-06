namespace Now

open System
open Sql
open Eff

(*
    Domain
*)

type MigrationVersion = MigrationVersion of id : Guid * version : int 

type Migration = Migration of MigrationVersion * (Database -> Sql<unit>)


type MigrationHistoryRecord = {
    runTime : DateTimeOffset
    version : MigrationVersion
}


module Migration =
    
    let create (guid : string) version run = Migration (MigrationVersion (Guid.Parse guid, version), run)
    
    let migrationVersion (Migration (version, _)) = version
    let run (Migration (_, run)) = run
    
    let version (MigrationVersion (_, version)) = version

    (*
        Error
    *)

    type Error =
        | FileError of Fs.Error
        | SqlError of Sql.Error
        | CurrentVersionMissingFromMigrations of MigrationVersion
        | MigrationSequenceInvalid of MigrationVersion
        | MigrationFailed of MigrationVersion * exn


    (*
        DSL
    *)

    type Instruction<'a> =
    | GetGlobalMigrationHistory of GlobalEnvironment * (MigrationHistoryRecord list -> 'a)
    | GetLocalMigrationHistory of LocalEnvironment * (MigrationHistoryRecord list -> 'a)
    | GetGlobalMigrationVersion of GlobalEnvironment * (MigrationVersion option -> 'a)
    | GetLocalMigrationVersion of LocalEnvironment * (MigrationVersion option -> 'a)
    | RunGlobalMigrations of GlobalEnvironment * Migration list * 'a
    | RunLocalMigrations of LocalEnvironment * Migration list * 'a

    and Program<'a> =
    | Free of Instruction<Program<'a>>
    | Pure of 'a

    let private mapI f = function
    | GetGlobalMigrationHistory(env, next) -> GetGlobalMigrationHistory(env, next >> f)
    | GetLocalMigrationHistory(env, next) -> GetLocalMigrationHistory(env, next >> f)
    | GetGlobalMigrationVersion(env, next) -> GetGlobalMigrationVersion(env, next >> f)
    | GetLocalMigrationVersion(env, next) -> GetLocalMigrationVersion(env, next >> f)
    | RunGlobalMigrations(env, migrations, next) -> RunGlobalMigrations(env, migrations, next |> f)
    | RunLocalMigrations(env, migrations, next) -> RunLocalMigrations(env, migrations, next |> f)

    let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x

    let getGlobalMigrationHistory env = Free(GetGlobalMigrationHistory(env, Pure))
    let getLocalMigrationHistory env = Free(GetLocalMigrationHistory(env, Pure))
    let getGlobalMigrationVersion env = Free(GetGlobalMigrationVersion(env, Pure))
    let getLocalMigrationVersion env = Free(GetLocalMigrationVersion(env, Pure))
    let runGlobalMigrations env migrations = Free(RunGlobalMigrations(env, migrations, Pure()))
    let runLocalMigrations env migrations = Free(RunLocalMigrations(env, migrations, Pure()))


    (*
        Interpreter
    *)

    open FSharpPlus.Builders
    open FSharpPlus.Operators

    let private getMigrationHistory db =
        execQuery
            db
            ( Query
                ( """
                  SELECT
                      [RunDate] 
                    , [Id]
                    , [Version]
                  FROM [MigrationHistory]
                  ORDER BY [Version]
                  """
                , []
                )
            )
        |> (mapReader
                (fun reader ->
                    { runTime = reader.GetString 0 |> DateTimeOffset.Parse
                      version = MigrationVersion (reader.GetGuid 1, reader.GetInt32 2) }
                )
            )

    let private getMigrationVersion db =
        execQuery
            db
            ( Query
                ( """
                  SELECT
                      [Id]
                    , [Version]
                  FROM [MigrationHistory]
                  ORDER BY [Version] DESC
                  LIMIT 1
                  """
                , []
                )
            )
        |> (mapReader (fun reader -> MigrationVersion (reader.GetGuid 0, reader.GetInt32 1)))
        |> map List.tryHead

    let rec runMigrationsBody db = function
    | Migration (MigrationVersion (id, version), run)::migrations ->
        monad {
            do!
                runInTransaction
                    db
                    ( execNonQuery
                        db
                        ( Query
                            ( """
                              INSERT INTO [MigrationHistory]
                                ( [Id] 
                                , [Version]
                                , [RunDate]
                               )
                              VALUES
                                ( @id 
                                , @version
                                , @runDate
                                )
                              """
                            , [
                                param "@id" (id.ToString())
                                param "@version" version
                                param "@runDate" DateTimeOffset.UtcNow
                              ]
                            )
                        )
                      *> run db
                    )
            return! runMigrationsBody db migrations
        }
    | _ -> result ()

    let private runMigrations db migrations =
        monad {
            let firstOutOfOrderMigration =
                migrations
                |> map migrationVersion
                |> tryHead
                |> filter (fun (MigrationVersion (_, version)) -> version <> 1)
                <|> (migrations
                    |> map migrationVersion
                    |> List.pairwise
                    |> tryFind (fun (MigrationVersion (_, prevVersion), MigrationVersion (_, nextVersion)) -> nextVersion - prevVersion <> 1)
                    |> map snd)
            
            match firstOutOfOrderMigration with
            | Some x -> do! liftRes <| Error (MigrationSequenceInvalid x)
            | None -> return ()
            
            let! currentVersion = liftSql <| getMigrationVersion db
                
            match currentVersion with
            | Some v when migrations |> map migrationVersion |> List.contains v |> not ->
                do! liftRes <| Error (CurrentVersionMissingFromMigrations v)
            | _ -> return ()

            let migrationsSql =
                migrations
                |> List.filter (fun x -> version (migrationVersion x) > (currentVersion |> Option.map version |> Option.defaultValue 0))
                |> runMigrationsBody db
                
            do! liftSql migrationsSql
        }
    
    let rec interpret = function
        | Pure a -> result a
        | Free(GetGlobalMigrationHistory(env, next)) ->
            getMigrationHistory env.database |> liftSql |> map next >>= interpret
        | Free(GetLocalMigrationHistory(env, next)) ->
            getMigrationHistory env.database |> liftSql |> map next >>= interpret
        | Free(GetGlobalMigrationVersion(env, next)) ->
            getMigrationVersion env.database |> liftSql |> map next >>= interpret
        | Free(GetLocalMigrationVersion(env, next)) ->
            getMigrationVersion env.database |> liftSql |> map next >>= interpret
        | Free(RunGlobalMigrations(env, migrations, next)) ->
            runMigrations env.database migrations |> map (konst next) >>= interpret
        | Free(RunLocalMigrations(env, migrations, next)) ->
            runMigrations env.database migrations |> map (konst next) >>= interpret