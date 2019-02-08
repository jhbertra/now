﻿namespace Now

open FSharpPlus.Operators

module Now =

    (*
        Domain
    *)

    type RunMigrationsCommand =
        | GetTasks
        | CreateTask of string
        | StartTask of string
        | RenameTask of string * string
        | DeleteTask of string
        | Version

    type Command =
        | Install
        | Uninstall
        | WithMigrations of RunMigrationsCommand

    type Error =
        | FileError of Fs.Error
        | SqlError of Sql.Error
        | CommandLineInvalid
        | TaskError of Task.Error
        | EnvironmentError of Env.Error
        | MigrationError of Migration.Error

    (*
        DSL
    *)

    type NowInstruction<'a> =
        | RunConsole of Console.Console<'a>
        | RunTask of Task.Program<'a>
        | RunMigration of Migration.Program<'a>
        | RunEnvironment of Env.Program<'a>
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
        | Free (RunEnvironment x) -> Env.interpret x |> Eff.mapError EnvironmentError >>= interpret
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
        | Nil -> Ok (WithMigrations GetTasks)
        | Arg name -> Ok (WithMigrations (CreateTask name))
        | Flag "d" "delete" (true, Arg name) -> Ok (WithMigrations (DeleteTask name))
        | Opt "m" "rename" (Some newName, Arg name) -> Ok  (WithMigrations (RenameTask (name, newName)))
    //    | ["start"; name] when (name.StartsWith("-")) |> not -> Ok (WithMigrations (StartTask name))
        | _ -> Error CommandLineInvalid

    let parseCommand = function
        | Command "install" Nil -> Ok Install
        | Command "start" (Arg name) -> StartTask name |> WithMigrations |> Ok
        | Command "task" opts -> parseTask opts
        | Command "uninstall" Nil -> Ok Uninstall
        | Command "version" Nil -> WithMigrations Version |> Ok
        | _ -> Error CommandLineInvalid

    let renderError a = sprintf "%A" a

//    let renderEnvironmentError = function
//    | CommandLineInvalid -> "Command Line Invalid"
//    | TaskCommandInvalid -> "Task Command Line Invalid"
//    | TaskExists name -> sprintf "Task exists: %s" name
//    | TaskNotFound name -> sprintf "Task not found: %s" name
//    | EnvironmentError (Env.IoError exn) -> sprintf "IO Error: %s" exn.Message
//    | EnvironmentError (Env.DirExists) -> "The global Now directory already exists"
//    | EnvironmentError (Env.DirExists) -> "A local Now directory already exists"
//    | EnvironmentError (Env.DirMissing) -> "Now is not installed, to install run 'now init -g'"
//    | EnvironmentError (Env.DirMissing) -> "Not in a Now project, to create one run 'now init'"
//    | MigrationError (Migration.FileError exn) -> sprintf "IO Error: %A" exn
//    | MigrationError (Migration.SqlError exn) -> sprintf "SQL Error: %A" exn
//    | MigrationError (Migration.CurrentVersionMissingFromMigrations version) -> sprintf "The current database version %A is not in the migration sequence" version
//    | MigrationError (Migration.MigrationSequenceInvalid version) -> sprintf "Unexpected migration version: %A" version
//    | MigrationError (Migration.MigrationFailed (version, e)) -> sprintf "Error running migration %A: %s" version e.Message
    
    open Now.Console
    open Now.Task
    open Now.Env
    open Now.Migration
    open Now.Migrations
    open FSharpPlus.Builders

    let runCommandPostMigrations = function
        | RunMigrationsCommand.CreateTask name ->
            monad {
                let! env = liftEnv readEnv
                return! liftTsk <| createTask env name
            }
        | RunMigrationsCommand.DeleteTask name ->
            monad {
                let! env = liftEnv readEnv
                return! liftTsk <| deleteTask env name
            }
        | RunMigrationsCommand.RenameTask (oldName, newName) ->
            monad {
                let! env = liftEnv readEnv
                return! liftTsk <| renameTask env oldName newName
            }
        | RunMigrationsCommand.GetTasks ->
            monad {
                let! env = liftEnv readEnv
                let! tasks = liftTsk <| getTasks env
                if List.isEmpty tasks then
                    do! liftCon <| writeLine "No current tasks, to create one run 'now task <name>'"
                else
                    for task in tasks do
                        do! liftCon <| writeLine (sprintf "%s" task.name)
                return ()
            }
        | RunMigrationsCommand.StartTask name ->
            monad {
                let! env = liftEnv readEnv
                let! task = liftTsk <| getTask env name
                let! activeTask = liftTsk <| getActiveTask env
                
                if Option.isSome activeTask then
                    do! liftTsk <| setActiveTask env None
                
                do! liftTsk <| setActiveTask env (Some task)
            }
        | Version ->
            monad {
                let! env = liftEnv readEnv
                let! globalVersion = liftMig <| getMigrationVersion env
                match globalVersion with
                | Some (MigrationVersion (id, version)) -> 
                    do! liftCon <| writeLine (sprintf "Database Version: %d.%s" version (id.ToString()))
                | None ->
                    do! liftCon <| writeLine " database has no migration history"
            }
        
    let runCommand = function
        | Command.Install -> liftEnv install
        | Command.Uninstall -> liftEnv uninstall
        | WithMigrations cmd ->
            monad {
                let! env = liftEnv readEnv
                do! liftMig
                    <| runMigrations
                        env
                        [
                            Migration.create "FD2D8875-2969-4BD9-8BE3-A230E286D15D" 1 Now1_AddTask.run
                        ]
                return! runCommandPostMigrations cmd
            }
