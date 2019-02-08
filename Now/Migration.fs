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
    | GetMigrationHistory of Env * (MigrationHistoryRecord list -> 'a)
    | GetMigrationVersion of Env * (MigrationVersion option -> 'a)
    | RunMigrations of Env * Migration list * 'a

    and Program<'a> =
    | Free of Instruction<Program<'a>>
    | Pure of 'a

    let private mapI f = function
    | GetMigrationHistory(env, next) -> GetMigrationHistory(env, next >> f)
    | GetMigrationVersion(env, next) -> GetMigrationVersion(env, next >> f)
    | RunMigrations(env, migrations, next) -> RunMigrations(env, migrations, next |> f)

    let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x

    let getMigrationHistory env = Free(GetMigrationHistory(env, Pure))
    let getMigrationVersion env = Free(GetMigrationVersion(env, Pure))
    let runMigrations env migrations = Free(RunMigrations(env, migrations, Pure()))


    (*
        Interpreter
    *)

    open FSharpPlus.Builders
    open FSharpPlus.Operators

    let private getMigrationVersionSql db =
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
    
    let rec interpret = function
        | Pure a -> result a

        | Free(GetMigrationHistory(env, next)) ->
            execQuery
                env.database
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
            |> liftSql
            |> mapError SqlError
            |> map next
            >>= interpret

        | Free(GetMigrationVersion(env, next)) ->
            getMigrationVersionSql env.database |> liftSql |> mapError SqlError |> map next >>= interpret

        | Free(RunMigrations(env, migrations, next)) ->
            let rec runMigrations' db = function
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
                        return! runMigrations' db migrations
                    }
                | _ -> result ()

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
                
                let! currentVersion = liftSql <| getMigrationVersionSql env.database |> mapError SqlError
                    
                match currentVersion with
                | Some v when migrations |> map migrationVersion |> List.contains v |> not ->
                    do! liftRes <| Error (CurrentVersionMissingFromMigrations v)
                | _ -> return ()
    
                let migrationsSql =
                    migrations
                    |> List.filter (fun x -> version (migrationVersion x) > (currentVersion |> Option.map version |> Option.defaultValue 0))
                    |> runMigrations' env.database
                    
                do! liftSql migrationsSql |> mapError SqlError
            }
            |> map (konst next)
            >>= interpret
