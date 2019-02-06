namespace Now


module Now =

    (*
        Domain
    *)

    type RunMigrationsCommand =
    | GetTasks
    | CreateTask of string
    | RenameTask of string * string
    | DeleteTask of string
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
    | FileError of Fs.Error
    | SqlError of Sql.Error
    | CommandLineInvalid
    | EnvironmentError of Environment.Error
    | MigrationError of Migration.Error
    
    let unwrapEffErrpr = function
    | Eff.FileError e -> FileError e
    | Eff.SqlError e -> SqlError e
    | Eff.UserError e -> e

    (*
        DSL
    *)

    type NowT<'a> =
    | Run of Migration.Program<Environment.Program<Result<'a, Error>>>

    type Program<'a> =
    | Free of NowT<Program<'a>>
    | Pure of 'a

    let mapStack f = (Migration.map (Environment.map (Result.map f)))
    let mapT f (Run p) = mapStack f p |> Run

    let rec bind f = function
    | Free x -> x |> mapT (bind f) |> Free
    | Pure x -> f x

    type Program<'a> with

        static member Return x = Pure x

        static member (>>=) (x, f) = bind f x

    let wrap x = x |> Run |> mapT Pure |> Free
    let liftDat x = wrap <| Migration.map (Environment.Pure << Ok) x
    let liftEnv x = wrap <| Migration.Pure (Environment.map Ok x)
    let liftRes x = wrap <| Migration.Pure (Environment.Pure x)

    (*
        Interpreter
    *)

    open FSharpPlus.Operators

    let rec interpret = function
    | Pure a -> result a
    | Free(Run p) ->
        p
        |> (Migration.interpret >> Eff.mapError MigrationError)
        >>= (Environment.interpret >> Eff.mapError EnvironmentError)
        >>= Eff.liftRes
        >>= interpret

    (*
        Public API
    *)

//    let parseTask = function
//    | [] -> Ok (WithLocalMigrations GetTasks)
//    | ["create"; name] when (name.StartsWith("-")) |> not -> Ok (WithLocalMigrations (CreateTask name))
//    | ["delete"; name] when (name.StartsWith("-")) |> not -> Ok (WithLocalMigrations (DeleteTask name))
//    | ["rename"; oldName; newName] when
//        (oldName.StartsWith("-")) |> not && (newName.StartsWith("-")) |> not ->
//            Ok (WithLocalMigrations (RenameTask (oldName, newName)))
//    | ["start"; name] when (name.StartsWith("-")) |> not -> Ok (WithLocalMigrations (StartTask name))
//    | _ -> Error TaskCommandInvalid

    let parseCommand = function
    | "init" :: opts when opts |> List.contains "-g" -> Ok InitGlobal
    | "init" :: opts -> Ok InitLocal
//    | "task" :: opts -> parseTask opts
    | "uninstall" :: opts when opts |> List.contains "-g" -> Ok UninstallGlobal
    | "uninstall" :: opts -> Ok UninstallLocal
    | "version" :: opts when opts |> List.contains "-g" -> WithGlobalMigrations VersionGlobal |> Ok
    | "version" :: opts -> WithLocalMigrations VersionLocal |> Ok
    | _ -> Error CommandLineInvalid

    let renderEnvironmentError a = sprintf "%A" a

//    let renderEnvironmentError = function
//    | CommandLineInvalid -> "Command Line Invalid"
//    | TaskCommandInvalid -> "Task Command Line Invalid"
//    | TaskExists name -> sprintf "Task exists: %s" name
//    | TaskNotFound name -> sprintf "Task not found: %s" name
//    | EnvironmentError (Environment.IoError exn) -> sprintf "IO Error: %s" exn.Message
//    | EnvironmentError (Environment.GlobalDirExists) -> "The global Now directory already exists"
//    | EnvironmentError (Environment.LocalDirExists) -> "A local Now directory already exists"
//    | EnvironmentError (Environment.GlobalDirMissing) -> "Now is not installed, to install run 'now init -g'"
//    | EnvironmentError (Environment.LocalDirMissing) -> "Not in a Now project, to create one run 'now init'"
//    | MigrationError (Migration.FileError exn) -> sprintf "IO Error: %A" exn
//    | MigrationError (Migration.SqlError exn) -> sprintf "SQL Error: %A" exn
//    | MigrationError (Migration.CurrentVersionMissingFromMigrations version) -> sprintf "The current database version %A is not in the migration sequence" version
//    | MigrationError (Migration.MigrationSequenceInvalid version) -> sprintf "Unexpected migration version: %A" version
//    | MigrationError (Migration.MigrationFailed (version, e)) -> sprintf "Error running migration %A: %s" version e.Message
    
    open Now.Environment
    open Now.Migration
    open Now.Console
    open FSharpPlus.Builders

    let runCommandPostMigrations = function
//    | RunMigrationsCommand.CreateTask name ->
//        monad {
//            let! env = liftEnv readLocalEnvironment
//            let! task = liftTsk <| getTask env name
//            if Option.isSome task then
//                return! liftRes <| Error (TaskExists name)
//            else
//                do! liftTsk <| createTask env name
//        }
//    | RunMigrationsCommand.DeleteTask name ->
//        monad {
//            let! env = liftEnv readLocalEnvironment
//            let! task = liftTsk <| getTask env name
//            if Option.isNone task then
//                return! liftRes <| Error (TaskNotFound name)
//            else
//                do! liftTsk <| deleteTask env name
//        }
//    | RunMigrationsCommand.RenameTask (oldName, newName) ->
//        monad {
//            let! env = liftEnv readLocalEnvironment
//            let! task = liftTsk <| getTask env oldName
//            let! newTask = liftTsk <| getTask env newName
//            if Option.isNone task then
//                return! liftRes <| Error (TaskNotFound oldName)
//            elif Option.isSome newTask then
//                return! liftRes <| Error (TaskExists newName)
//            else
//                do! liftTsk <| renameTask env oldName newName
//        }
//    | RunMigrationsCommand.GetTasks ->
//        monad {
//            let! env = liftEnv readLocalEnvironment
//            let! tasks = liftTsk <| getTasks env
//            if List.isEmpty tasks then
//                do! liftCL <| writeLine "No current tasks, to create one run 'now task <name>'"
//            else
//                for task in tasks do
//                    do! liftCL <| writeLine task.name
//        }
    | VersionGlobal ->
        monad {
            let! env = liftEnv readGlobalEnvironment
            let! globalVersion = liftDat <| getGlobalMigrationVersion env
            match globalVersion with
            | Some (MigrationVersion (id, version)) -> 
                printfn "Database Version: %d.%s" version (id.ToString())
            | None ->
                printfn "Global database has no migration history"
        }
    | VersionLocal ->
        monad {
            let! env = liftEnv readLocalEnvironment
            let! globalVersion = liftDat <| getGlobalMigrationVersion env.globalEnv
            let! localVersion = liftDat <| getLocalMigrationVersion env
            match globalVersion with
            | Some (MigrationVersion (id, version)) -> 
                printfn "Global database Version: %d.%s" version (id.ToString())
            | None ->
                printfn "Global database has no migration history"
            match localVersion with
            | Some (MigrationVersion (id, version)) -> 
                printfn "Local database Version: %d.%s" version (id.ToString())
            | None ->
                printfn "Local database has no migration history"
        }
        
    open Now.Migrations
    
    let runCommand = function
    | Command.InitGlobal -> liftEnv initGlobal
    | Command.InitLocal -> liftEnv initLocal
    | Command.UninstallGlobal -> liftEnv uninstallGlobal
    | Command.UninstallLocal -> liftEnv uninstallLocal
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
                        Migration.create "FD2D8875-2969-4BD9-8BE3-A230E286D15D" 1 Local1_AddTask.run
                    ]
            return! runCommandPostMigrations cmd
        }
