namespace Now
open System

(*
    Domain
*)

type MigrationVersion = {
    id : Guid
    version : int
} 


type MigrationHistoryRecord = {
    runTime : DateTimeOffset
    migrationVersion : MigrationVersion
}


module Data =
    open System.Data.SQLite

    (*
        Error
    *)

    type Error =
    | IoError of exn
    | SqlError of SQLiteException
    | GlobalDatabaseExists
    | LocalDatabaseExists
    | GlobalDatabaseMissing
    | LocalDatabaseMissing
    | CurrentVersionMissingFromMigrations of MigrationVersion
    | MigrationSequenceInvalid of MigrationVersion
    | MigrationFailed of MigrationVersion * exn


    (*
        DSL
    *)

    type Instruction<'a, 'b> =
    | GetGlobalMigrationHistory of GlobalEnvironment * (MigrationHistoryRecord list -> 'a)
    | GetLocalMigrationHistory of LocalEnvironment * (MigrationHistoryRecord list -> 'a)
    | GetGlobalMigrationVersion of GlobalEnvironment * (MigrationVersion option -> 'a)
    | GetLocalMigrationVersion of LocalEnvironment * (MigrationVersion option -> 'a)
    | InitGlobalData of GlobalEnvironment * 'a
    | InitLocalData of LocalEnvironment * 'a
    | RunGlobalMigrations of GlobalEnvironment * (MigrationVersion * (SQLiteConnection -> unit)) list * 'a
    | RunLocalMigrations of LocalEnvironment * (MigrationVersion * (SQLiteConnection -> unit)) list * 'a
    | UninstallGlobalData of GlobalEnvironment * 'a
    | UninstallLocalData of LocalEnvironment * 'a

    and Program<'a, 'b> =
    | Free of Instruction<Program<'a, 'b>, 'b>
    | Pure of 'a

    let private mapI f = function
    | GetGlobalMigrationHistory(env, next) -> GetGlobalMigrationHistory(env, next >> f)
    | GetLocalMigrationHistory(env, next) -> GetLocalMigrationHistory(env, next >> f)
    | GetGlobalMigrationVersion(env, next) -> GetGlobalMigrationVersion(env, next >> f)
    | GetLocalMigrationVersion(env, next) -> GetLocalMigrationVersion(env, next >> f)
    | InitGlobalData(env, next) -> InitGlobalData(env, next |> f)
    | InitLocalData(env, next) -> InitLocalData(env, next |> f)
    | RunGlobalMigrations(env, migrations, next) -> RunGlobalMigrations(env, migrations, next |> f)
    | RunLocalMigrations(env, migrations, next) -> RunLocalMigrations(env, migrations, next |> f)
    | UninstallGlobalData(env, next) -> UninstallGlobalData(env, next |> f)
    | UninstallLocalData(env, next) -> UninstallLocalData(env, next |> f)

    let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a, 'b> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x

    let getGlobalMigrationHistory env = Free(GetGlobalMigrationHistory(env, Pure))
    let getLocalMigrationHistory env = Free(GetLocalMigrationHistory(env, Pure))
    let getGlobalMigrationVersion env = Free(GetGlobalMigrationVersion(env, Pure))
    let getLocalMigrationVersion env = Free(GetLocalMigrationVersion(env, Pure))
    let initGlobalData env = Free(InitGlobalData(env, Pure()))
    let initLocalData env = Free(InitLocalData(env, Pure()))
    let runGlobalMigrations env migrations = Free(RunGlobalMigrations(env, migrations, Pure()))
    let runLocalMigrations env migrations = Free(RunLocalMigrations(env, migrations, Pure()))
    let uninstallGlobalData env = Free(UninstallGlobalData(env, Pure()))
    let uninstallLocalData env = Free(UninstallLocalData(env, Pure()))


    (*
        Interpreter
    *)

    open System.IO
    open FSharpPlus.Builders
    open FSharpPlus.Operators

    let buildDatabaseFile dir = Path.Combine(dir, "now.sqlite")
    let buildConnectionString file = sprintf "Data Source=%s;Version=3;" file

    let private initDb e dbFile =
        try
            if File.Exists dbFile then    
                Error e
            else
                let connectionString = buildConnectionString dbFile
                let connection = new SQLiteConnection(connectionString)
                connection.Open()
                use command = connection.CreateCommand()
                command.CommandText <-
                    """
                    CREATE TABLE [MigrationHistory]
                      ( [Id] GUID NOT NULL PRIMARY KEY 
                      , [Version] INT NOT NULL UNIQUE
                      , [RunDate] DATETIMEOFFSET NOT NULL
                      )
                    """
                command.ExecuteNonQuery() |> ignore
                connection.Close()
                Ok ()
        with
        | :? SQLiteException as e -> Error(SqlError e)
        | e -> Error(IoError e)

    let executeNonQuery sql e parameters dbFile =
        try
            if not (File.Exists dbFile) then    
                Error e
            else
                let connectionString = buildConnectionString dbFile
                let connection = new SQLiteConnection(connectionString)
                connection.Open()
                use command = connection.CreateCommand()
                command.CommandText <- sql
                for (name, value) in parameters do
                    command.Parameters.AddWithValue(name, value) |> ignore
                let i = command.ExecuteNonQuery()
                connection.Close()
                Ok i
        with
        | :? SQLiteException as e -> Error(SqlError e)
        | e -> Error(IoError e)

    let private executeQuery sql e map parameters dbFile =
        try
            if not (File.Exists dbFile) then    
                Error e
            else
                let connectionString = buildConnectionString dbFile
                let connection = new SQLiteConnection(connectionString)
                connection.Open()
                use command = connection.CreateCommand()
                command.CommandText <- sql
                for (name, value) in parameters do
                    command.Parameters.AddWithValue(name, value) |> ignore
                let r = command.ExecuteReader()
                let mutable results = []
                while r.Read() do results <- (map r)::results
                connection.Close()
                Ok results
        with
        | :? SQLiteException as e -> Error(SqlError e)
        | e -> Error(IoError e)

    let executeLocalNonQuery sql = executeNonQuery sql LocalDatabaseMissing
    let executeGlobalNonQuery sql = executeNonQuery sql GlobalDatabaseMissing

    let executeLocalQuery sql = executeQuery sql LocalDatabaseMissing
    let executeGlobalQuery sql = executeQuery sql GlobalDatabaseMissing

    let private removeDb dbFile = Result.catch (fun () -> File.Delete dbFile) |> Result.mapError IoError

    let private getMigrationHistory =
        executeQuery
            """
            SELECT
                [RunDate] 
              , [Id]
              , [Version]
            FROM [MigrationHistory]
            ORDER BY [Version]
            """
            LocalDatabaseMissing
            (fun reader ->
                { runTime = reader.GetString 0 |> DateTimeOffset.Parse
                  migrationVersion =
                    { id = reader.GetGuid 1
                      version = reader.GetInt32 2 } }
            )
            []

    let private getMigrationVersion =
        executeQuery
            """
            SELECT
                [Id]
              , [Version]
            FROM [MigrationHistory]
            ORDER BY [Version] DESC
            LIMIT 1
            """
            LocalDatabaseMissing
            (fun reader ->
                { id = reader.GetGuid 0
                  version = reader.GetInt32 1 }
            )
            []
        >> Result.map List.tryHead

    let rec runMigrationsBody (connection : SQLiteConnection) = function
    | (version, migration)::migrations ->
        use transaction = connection.BeginTransaction()
        try
            migration connection
            use command = connection.CreateCommand()
            command.CommandText <-
                """
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
            command.Parameters.AddWithValue("@id", version.id.ToString()) |> ignore
            command.Parameters.AddWithValue("@version", version.version) |> ignore
            command.Parameters.AddWithValue("@runDate", DateTimeOffset.UtcNow) |> ignore
            command.ExecuteNonQuery() |> ignore
            transaction.Commit()
            Ok () *> runMigrationsBody connection migrations
        with
        | e ->
            transaction.Rollback()
            Error (MigrationFailed (version, e))
    | _ -> Ok ()

    let private runMigrations e migrations dbFile =
        monad {
            let firstOutOfOrderMigration =
                migrations
                |> map fst
                |> tryHead
                |> filter (fun x -> x.version <> 1)
                <|> (migrations
                    |> map fst
                    |> List.pairwise
                    |> tryFind (fun (prev, next) -> next.version - prev.version <> 1)
                    |> map snd)
            do!
                match firstOutOfOrderMigration with
                | Some x -> Error (MigrationSequenceInvalid x)
                | None -> Ok ()

            let! currentVersion = getMigrationVersion dbFile
                
            do!
                match currentVersion with
                | Some version when migrations |> map fst |> List.contains version |> not -> Error (CurrentVersionMissingFromMigrations version)
                | _ -> Ok ()

            try
                if not (File.Exists dbFile) then
                    return! Error e
                else
                    let connectionString = buildConnectionString dbFile
                    let connection = new SQLiteConnection(connectionString)
                    connection.Open()
                    do!
                        migrations 
                        |> filter (fun (x, _) -> x.version > (currentVersion |> Option.map (fun y -> y.version) |> Option.defaultValue 0))
                        |> runMigrationsBody connection
                    connection.Close()
            with
            | :? SQLiteException as e -> return! Error(SqlError e)
            | e -> return! Error(IoError e)
        }

    let rec interpret = function
    | Pure a -> Ok a

    | Free(GetGlobalMigrationHistory(env, next)) ->
        buildDatabaseFile env.globalDir
        |> getMigrationHistory
        |> Result.map next
        >>= interpret

    | Free(GetLocalMigrationHistory(env, next)) ->
        buildDatabaseFile env.localDir
        |> getMigrationHistory
        |> Result.map next
        >>= interpret

    | Free(GetGlobalMigrationVersion(env, next)) ->
        buildDatabaseFile env.globalDir
        |> getMigrationVersion
        |> Result.map next
        >>= interpret

    | Free(GetLocalMigrationVersion(env, next)) ->
        buildDatabaseFile env.localDir
        |> getMigrationVersion
        |> Result.map next
        >>= interpret

    | Free(InitGlobalData(env, next)) ->
        buildDatabaseFile env.globalDir
        |> initDb GlobalDatabaseExists
        |> Result.map (konst next)
        >>= interpret

    | Free(InitLocalData(env, next)) ->
        buildDatabaseFile env.localDir
        |> initDb LocalDatabaseExists
        |> Result.map (konst next)
        >>= interpret

    | Free(RunGlobalMigrations(env, migrations, next)) ->
        buildDatabaseFile env.globalDir
        |> runMigrations LocalDatabaseExists migrations
        |> Result.map (konst next)
        >>= interpret

    | Free(RunLocalMigrations(env, migrations, next)) ->
        buildDatabaseFile env.localDir
        |> runMigrations LocalDatabaseExists migrations
        |> Result.map (konst next)
        >>= interpret

    | Free(UninstallGlobalData(env, next)) ->
        buildDatabaseFile env.globalDir
        |> removeDb
        |> Result.map (konst next)
        >>= interpret

    | Free(UninstallLocalData(env, next)) ->
        buildDatabaseFile env.localDir
        |> removeDb
        |> Result.map (konst next)
        >>= interpret