namespace Now

(*
    Domain
*)


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


    (*
        DSL
    *)

    type Instruction<'a> =
    | InitGlobalData of GlobalEnvironment * 'a
    | InitLocalData of LocalEnvironment * 'a
    | UninstallGlobalData of GlobalEnvironment * 'a
    | UninstallLocalData of LocalEnvironment * 'a

    type Program<'a> =
    | Free of Instruction<Program<'a>>
    | Pure of 'a

    let private mapI f = function
    | InitGlobalData(env, next) -> InitGlobalData(env, next |> f)
    | InitLocalData(env, next) -> InitLocalData(env, next |> f)
    | UninstallGlobalData(env, next) -> UninstallGlobalData(env, next |> f)
    | UninstallLocalData(env, next) -> UninstallLocalData(env, next |> f)

    let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x

    let initGlobalData env = Free(InitGlobalData(env, Pure()))
    let initLocalData env = Free(InitLocalData(env, Pure()))
    let uninstallGlobalData env = Free(UninstallGlobalData(env, Pure()))
    let uninstallLocalData env = Free(UninstallLocalData(env, Pure()))



    (*
        Interpreter
    *)

    open System.IO
    open FSharpPlus.Operators

    let private buildDatabaseFile dir = Path.Combine(dir, "now.sqlite")
    let private buildConnectionString file = sprintf "Data Source=%s;Version=3;" file

    let private initDb e dbFile =
        try
            if File.Exists dbFile then    
                Error e
            else
                let connectionString = buildConnectionString dbFile
                let sqliteConnection = new SQLiteConnection(connectionString)
                sqliteConnection.Open()
                let command = sqliteConnection.CreateCommand()
                command.CommandText <-
                    """
                    CREATE TABLE [MigrationHistory]
                      ( [Id] GUID NOT NULL PRIMARY KEY 
                      , [Version] INT NOT NULL UNIQUE
                      , [RunDate] DATETIMEOFFSET NOT NULL
                      )
                    """
                command.ExecuteNonQuery() |> ignore
                sqliteConnection.Close()
                Ok ()
        with
        | :? SQLiteException as e -> Error(SqlError e)
        | e -> Error(IoError e)

    let private removeDb dbFile = Result.catch (fun () -> File.Delete dbFile) |> Result.mapError IoError

    let rec interpret = function
    | Pure a -> Ok a
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
