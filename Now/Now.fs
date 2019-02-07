namespace Now

open FSharpPlus.Operators

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
        | TaskError of Task.Error
        | EnvironmentError of Environment.Error
        | MigrationError of Migration.Error

    (*
        DSL
    *)

    type NowInstruction<'a> =
        | RunConsole of Console.Console<'a>
        | RunTask of Task.Program<'a>
        | RunMigration of Migration.Program<'a>
        | RunEnvironment of Environment.Program<'a>
        | RunResult of Result<'a, Error>

    type Program<'a> =
        | Free of NowInstruction<Program<'a>>
        | Pure of 'a

    let mapI f = function
        | RunConsole x -> RunConsole (map f x)
        | RunTask x -> RunTask (map f x)
        | RunMigration x -> RunMigration (map f x)
        | RunEnvironment x -> RunEnvironment (map f x)
        | RunResult x -> RunResult (map f x)

    let rec bind f = function
        | Free x -> x |> mapI (bind f) |> Free
        | Pure x -> f x

    type Program<'a> with

        static member Return x = Pure x

        static member (>>=) (x, f) = bind f x

    let liftCon x = RunConsole x |> mapI Pure |> Free
    let liftTsk x = RunTask x |> mapI Pure |> Free
    let liftMig x = RunMigration x |> mapI Pure |> Free
    let liftEnv x = RunEnvironment x |> mapI Pure |> Free
    let liftRes x = RunResult x |> mapI Pure |> Free

    (*
        Interpreter
    *)

    let rec interpret = function
        | Pure a -> result a
        | Free (RunConsole x) -> Console.interpret x |> interpret
        | Free (RunTask x) -> Task.interpret x |> Eff.mapError TaskError >>= interpret
        | Free (RunMigration x) -> Migration.interpret x |> Eff.mapError MigrationError >>= interpret
        | Free (RunEnvironment x) -> Environment.interpret x |> Eff.mapError EnvironmentError >>= interpret
        | Free (RunResult x) -> Eff.liftRes x >>= interpret

    (*
        Public API
    *)
    
    let (|Args|_|) = function
        | args when List.forall (fun (value : string) -> not (value.StartsWith("-"))) args -> Some args
        | _ -> None
    
    let (|Arg|_|) = function
        | Args [arg] -> Some arg
        | _ -> None

    let (|Command|_|) (name : string) = function
        | x::opts when x = name && not (x.StartsWith("-")) -> Some opts 
        | _ -> None

    let (|Flag|_|) short long opts =
        let short = (sprintf "-%s" short)
        let long = (sprintf "-%s" long)
        if List.contains short opts || List.contains long opts then
            Some (true, List.except [short; long] opts)
        else
            Some (false, opts)
    
    let (|Nil|_|) = function
        | [] -> Some ()
        | _ -> None

    let (|Global|_|) = ``|Flag|_|`` "g" "global"

    let (|Opt|_|) short long opts =
        let short = sprintf "-%s" short
        let long = sprintf "--%s" long

        let rec remove = function    
            | (v, x::(value : string)::xs) when not (value.StartsWith("-")) && (x = short || x = long) ->
                remove (v <|> Some value, xs)
            | (v, x::xs) ->
                remove (v, xs) |> (fun (v, xs) -> (v, x::xs))
            | x -> x

        remove (None, opts) |> Some

    let parseTask = function
        | Nil -> Ok (WithLocalMigrations GetTasks)
        | Arg name -> Ok (WithLocalMigrations (CreateTask name))
        | Flag "d" "delete" (true, Arg name) -> Ok (WithLocalMigrations (DeleteTask name))
        | Opt "m" "rename" (Some newName, Arg name) -> Ok  (WithLocalMigrations (RenameTask (name, newName)))
    //    | ["start"; name] when (name.StartsWith("-")) |> not -> Ok (WithLocalMigrations (StartTask name))
        | _ -> Error CommandLineInvalid

    let parseCommand = function
        | Command "init" (Global (true, Nil))-> Ok InitGlobal
        | Command "init" Nil -> Ok InitLocal
        | Command "task" opts -> parseTask opts
        | Command "uninstall" (Global (true, Nil)) -> Ok UninstallGlobal
        | Command "uninstall" Nil -> Ok UninstallLocal
        | Command "version" (Global (true, Nil)) -> WithGlobalMigrations VersionGlobal |> Ok
        | Command "version" Nil -> WithLocalMigrations VersionLocal |> Ok
        | _ -> Error CommandLineInvalid

    let renderError a = sprintf "%A" a

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
    
    open Now.Console
    open Now.Task
    open Now.Environment
    open Now.Migration
    open FSharpPlus.Builders

    let runCommandPostMigrations = function
        | RunMigrationsCommand.CreateTask name ->
            monad {
                let! env = liftEnv readLocalEnvironment
                return! liftTsk <| createTask env name
            }
        | RunMigrationsCommand.DeleteTask name ->
            monad {
                let! env = liftEnv readLocalEnvironment
                return! liftTsk <| deleteTask env name
            }
        | RunMigrationsCommand.RenameTask (oldName, newName) ->
            monad {
                let! env = liftEnv readLocalEnvironment
                return! liftTsk <| renameTask env oldName newName
            }
        | RunMigrationsCommand.GetTasks ->
            monad {
                let! env = liftEnv readLocalEnvironment
                let! tasks = liftTsk <| getTasks env
                if List.isEmpty tasks then
                    do! liftCon <| writeLine "No current tasks, to create one run 'now task <name>'"
                else
                    for task in tasks do
                        do! liftCon <| writeLine (sprintf "%s" task.name)
                return ()
            }
        | VersionGlobal ->
            monad {
                let! env = liftEnv readGlobalEnvironment
                let! globalVersion = liftMig <| getGlobalMigrationVersion env
                match globalVersion with
                | Some (MigrationVersion (id, version)) -> 
                    do! liftCon <| writeLine (sprintf "Database Version: %d.%s" version (id.ToString()))
                | None ->
                    do! liftCon <| writeLine "Global database has no migration history"
            }
        | VersionLocal ->
            monad {
                let! env = liftEnv readLocalEnvironment
                let! globalVersion = liftMig <| getGlobalMigrationVersion env.globalEnv
                let! localVersion = liftMig <| getLocalMigrationVersion env
                match globalVersion with
                | Some (MigrationVersion (id, version)) -> 
                    do! liftCon <| writeLine (sprintf "Global database Version: %d.%s" version (id.ToString()))
                | None ->
                    do! liftCon <| writeLine "Global database has no migration history"
                match localVersion with
                | Some (MigrationVersion (id, version)) -> 
                    do! liftCon <| writeLine (sprintf "Local database Version: %d.%s" version (id.ToString()))
                | None ->
                    do! liftCon <| writeLine "Local database has no migration history"
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
                do! liftMig <| runGlobalMigrations env []
                return! runCommandPostMigrations cmd
            }
        | WithLocalMigrations cmd ->
            monad {
                let! env = liftEnv readLocalEnvironment
                do! liftMig
                    <| runLocalMigrations
                        env
                        [
                            Migration.create "FD2D8875-2969-4BD9-8BE3-A230E286D15D" 1 Local1_AddTask.run
                        ]
                return! runCommandPostMigrations cmd
            }
