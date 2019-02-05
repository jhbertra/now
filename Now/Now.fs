namespace Now


module Now =

    (*
        Domain
    *)

    type RunMigrationsCommand =
    | GetTasks
    | CreateTask of string
    | VersionGlobal
    | VersionLocal

    type Command =
    | InitGlobal
    | InitLocal
    | UninstallGlobal
    | UninstallLocal
    | WithGlobalMigrations of RunMigrationsCommand
    | WithLocalMigrations of RunMigrationsCommand

    type Error =
    | EnvironmentError of Environment.Error
    | DataError of Data.Error
    | CommandLineInvalid
    | TaskCommandInvalid
    | TaskExists of string

    (*
        DSL
    *)

    type NowT<'a, 'b> =
    | Run of CommandLine.Program<Task.Program<Data.Program<Environment.Program<Result<'a, Error>>, 'b>>>

    type Program<'a, 'b> =
    | Free of NowT<Program<'a, 'b>, 'b>
    | Pure of 'a

    let mapStack f = CommandLine.map (Task.map (Data.map (Environment.map (Result.map f))))
    let mapT f (Run p) = mapStack f p |> Run

    let rec bind f = function
    | Free x -> x |> mapT (bind f) |> Free
    | Pure x -> f x

    type Program<'a, 'b> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x

    let wrap x = x |> Run |> mapT Pure |> Free
    let liftCL x = wrap <| CommandLine.map (Task.Pure << Data.Pure << Environment.Pure << Ok) x
    let liftTsk x = wrap <| CommandLine.Pure (Task.map (Data.Pure << Environment.Pure << Ok) x)
    let liftDat x = wrap <| CommandLine.Pure (Task.Pure (Data.map (Environment.Pure << Ok) x))
    let liftEnv x = wrap <| CommandLine.Pure (Task.Pure (Data.Pure (Environment.map Ok x)))
    let liftRes x = wrap <| CommandLine.Pure (Task.Pure (Data.Pure (Environment.Pure x)))

    (*
        Interpreter
    *)

    open FSharpPlus.Operators

    let rec interpret = function
    | Pure a -> Ok a
    | Free(Run p) ->
        p
        |> CommandLine.interpret
        |> (Task.interpret >> Result.mapError DataError)
        >>= (Data.interpret >> Result.mapError DataError)
        >>= (Environment.interpret >> Result.mapError EnvironmentError)
        >>= id
        >>= interpret

    (*
        Public API
    *)

    let parseTask = function
    | [] -> Ok (WithLocalMigrations GetTasks)
    | ["create"; name] when (name.StartsWith("-")) |> not -> Ok (WithLocalMigrations (CreateTask name))
    | _ -> Error TaskCommandInvalid

    let parseCommand = function
    | "init" :: opts when opts |> List.contains "-g" -> Ok InitGlobal
    | "init" :: opts -> Ok InitLocal
    | "task" :: opts -> parseTask opts
    | "uninstall" :: opts when opts |> List.contains "-g" -> Ok UninstallGlobal
    | "uninstall" :: opts -> Ok UninstallLocal
    | "version" :: opts when opts |> List.contains "-g" -> WithGlobalMigrations VersionGlobal |> Ok
    | "version" :: opts -> WithLocalMigrations VersionLocal |> Ok
    | _ -> Error CommandLineInvalid

    let renderEnvironmentError = function
    | CommandLineInvalid -> "Command Line Invalid"
    | TaskCommandInvalid -> "Task Command Line Invalid"
    | TaskExists name -> sprintf "Task exists: %s" name
    | EnvironmentError (Environment.IoError exn) -> sprintf "IO Error: %s" exn.Message
    | EnvironmentError (Environment.GlobalDirExists) -> "The global Now directory already exists"
    | EnvironmentError (Environment.LocalDirExists) -> "A local Now directory already exists"
    | EnvironmentError (Environment.GlobalDirMissing) -> "Now is not installed, to install run 'now init -g'"
    | EnvironmentError (Environment.LocalDirMissing) -> "Not in a Now project, to create one run 'now init'"
    | DataError (Data.IoError exn) -> sprintf "IO Error: %s" exn.Message
    | DataError (Data.SqlError exn) -> sprintf "SQL Error: %s" exn.Message
    | DataError (Data.GlobalDatabaseExists) -> "The global Now database already exists"
    | DataError (Data.LocalDatabaseExists) -> "A local Now database already exists"
    | DataError (Data.GlobalDatabaseMissing) -> "The global Now database is missing"
    | DataError (Data.LocalDatabaseMissing) -> "A local Now database is missing"
    | DataError (Data.CurrentVersionMissingFromMigrations version) -> sprintf "The current database version %A is not in the migration sequence" version
    | DataError (Data.MigrationSequenceInvalid version) -> sprintf "Unexpected migration version: %A" version
    | DataError (Data.MigrationFailed (version, e)) -> sprintf "Error running migration %A: %s" version e.Message
    
    open System
    open Now.Environment
    open Now.Data
    open Now.Task
    open Now.CommandLine
    open FSharpPlus.Builders

    let runCommandPostMigrations = function
    | RunMigrationsCommand.CreateTask name ->
        monad {
            let! env = liftEnv readLocalEnvironment
            let! task = liftTsk <| getTask env name
            if Option.isSome task then
                return! liftRes <| Error (TaskExists name)
            else
                do! liftTsk <| createTask env name
        }
    | RunMigrationsCommand.GetTasks ->
        monad {
            let! env = liftEnv readLocalEnvironment
            let! tasks = liftTsk <| getTasks env
            if List.isEmpty tasks then
                do! liftCL <| writeLine "No current tasks, to create one run 'now task <name>'"
            else
                for task in tasks do
                    do! liftCL <| writeLine task.name
        }
    | VersionGlobal ->
        monad {
            let! env = liftEnv readGlobalEnvironment
            let! globalVersion = liftDat <| getGlobalMigrationVersion env
            match globalVersion with
            | Some x -> 
                do! liftCL <| writeLine (sprintf "Database Version: %d.%s" x.version (x.id.ToString()))
            | None ->
                do! liftCL <| writeLine "Global database has no migration history"
        }
    | VersionLocal ->
        monad {
            let! env = liftEnv readLocalEnvironment
            let! globalVersion = liftDat <| getGlobalMigrationVersion env.globalEnv
            let! localVersion = liftDat <| getLocalMigrationVersion env
            match globalVersion with
            | Some x -> 
                do! liftCL <| writeLine (sprintf "Global database Version: %d.%s" x.version (x.id.ToString()))
            | None ->
                do! liftCL <| writeLine "Global database has no migration history"
            match localVersion with
            | Some x -> 
                do! liftCL <| writeLine (sprintf "Local database Version: %d.%s" x.version (x.id.ToString()))
            | None ->
                do! liftCL <| writeLine "Local database has no migration history"
        }
        
    open Now.Migrations
    
    let migration (id : string) version run = ({id = Guid.Parse(id); version = version}, run)
    
    let runCommand = function
    | InitGlobal ->
        monad {
            do! liftCL <| writeLine "Initializing global directory..."
            do! liftEnv initGlobalDir
            let! env = liftEnv readGlobalEnvironment
            do! liftCL <| writeLine "Initializing global database..."
            return! liftDat <| initGlobalData env
        }
    | InitLocal -> 
        monad {
            do! liftCL <| writeLine "Initializing local directory..."
            do! liftEnv initLocalDir
            let! env = liftEnv readLocalEnvironment
            do! liftCL <| writeLine "Initializing local database..."
            return! liftDat <| initLocalData env
        }
    | UninstallGlobal ->
        monad {
            let! env = liftEnv readGlobalEnvironment
            do! liftCL <| writeLine "Removing global database..."
            do! liftDat <| uninstallGlobalData env
            do! liftCL <| writeLine "Removing global directory..."
            return! liftEnv uninstallGlobalDir
        }
    | UninstallLocal -> 
        monad {
            let! env = liftEnv readLocalEnvironment
            do! liftCL <| writeLine "Removing local database..."
            do! liftDat <| uninstallLocalData env
            do! liftCL <| writeLine "Removing local directory..."
            return! liftEnv uninstallLocalDir
        }
    | WithGlobalMigrations cmd ->
        monad {
            let! env = liftEnv readGlobalEnvironment
            do! liftDat <| runGlobalMigrations env []
            return! runCommandPostMigrations cmd
        }
    | WithLocalMigrations cmd ->
        monad {
            let! env = liftEnv readLocalEnvironment
            do! liftDat
                <| runLocalMigrations
                    env
                    [
                        migration "FD2D8875-2969-4BD9-8BE3-A230E286D15D" 1 Local1_AddTask.run
                    ]
            return! runCommandPostMigrations cmd
        }
