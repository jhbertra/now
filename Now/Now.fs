namespace Now


module Now =

    (*
        Domain
    *)

    type Command =
    | InitGlobal
    | InitLocal
    | UninstallGlobal
    | UninstallLocal

    type Error =
    | EnvironmentError of Environment.Error
    | DataError of Data.Error
    | CommandLineInvalid

    (*
        DSL
    *)

    type NowT<'a> =
    | Run of CommandLine.Program<Data.Program<Environment.Program<'a>>>

    type Program<'a> =
    | Free of NowT<Program<'a>>
    | Pure of 'a

    let mapStack f = CommandLine.map (Data.map (Environment.map f))
    let mapT f (Run p) = mapStack f p |> Run

    let rec bind f = function
    | Free x -> x |> mapT (bind f) |> Free
    | Pure x -> f x

    type Program<'a> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x

    let wrap x = x |> Run |> mapT Pure |> Free
    let liftCL x = wrap <| CommandLine.map (Data.Pure << Environment.Pure) x
    let liftDat x = wrap <| CommandLine.Pure (Data.map Environment.Pure x)
    let liftEnv x = wrap <| CommandLine.Pure (Data.Pure x)

    (*
        Interpreter
    *)

    open FSharpPlus.Operators

    let rec interpret = function
    | Pure a -> Ok a
    | Free(Run p) ->
        p
        |> CommandLine.interpret
        |> (Data.interpret >> Result.mapError DataError)
        >>= (Environment.interpret >> Result.mapError EnvironmentError)
        >>= interpret

    (*
        Public API
    *)
    
    open Now.Environment
    open Now.Data
    open Now.CommandLine
    open FSharpPlus.Builders

    let parseCommand = function
    | "init" :: opts when opts |> List.contains "-g" -> Ok InitGlobal
    | "init" :: opts -> Ok InitLocal
    | "uninstall" :: opts when opts |> List.contains "-g" -> Ok UninstallGlobal
    | "uninstall" :: opts -> Ok UninstallLocal
    | _ -> Error CommandLineInvalid


    let renderEnvironmentError = function
    | CommandLineInvalid -> "Command Line Invalid"
    | EnvironmentError (Environment.IoError exn) -> sprintf "IO Error: %s" exn.Message
    | EnvironmentError (Environment.GlobalDirExists) -> "The global Now directory already exists"
    | EnvironmentError (Environment.LocalDirExists) -> "A local Now directory already exists"
    | EnvironmentError (Environment.GlobalDirMissing) -> "Now is not installed, to install run 'now init -g'"
    | EnvironmentError (Environment.LocalDirMissing) -> "Not in a Now project, to create one run 'now init'"
    | DataError (Data.IoError exn) -> sprintf "IO Error: %s" exn.Message
    | DataError (Data.SqlError exn) -> sprintf "SQL Error: %s" exn.Message
    | DataError (Data.GlobalDatabaseExists) -> "The global Now database already exists"
    | DataError (Data.LocalDatabaseExists) -> "A local Now database already exists"

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
