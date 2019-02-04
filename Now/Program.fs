open FSharpPlus.Operators
open Now


type Command =
| InitGlobal
| InitLocal


type Error =
| Environment of Environment.Error
| CommandLineInvalid


let parseCommand = function
| "init" :: opts when opts |> List.contains "-g" -> Ok InitGlobal
| "init" :: opts -> Ok InitLocal
| _ -> Error CommandLineInvalid


let runCommand = function
| InitGlobal -> Environment.initGlobalDir
| InitLocal -> Environment.initLocalDir

open Environment

let renderEnvironmentError = function
| CommandLineInvalid -> "Command Line Invalid"
| Environment (IoError exn) -> sprintf "IO Error: %s" exn.Message
| Environment (GlobalDirExists) -> "The global Now directory already exists"
| Environment (LocalDirExists) -> "A local Now directory already exists"


[<EntryPoint>]
let main argv =    
    let result =
        parseCommand (List.ofArray argv)
        |> map runCommand
        >>= (interpret >> Result.mapError Environment)
    match result with
    | Ok () -> 0
    | Error e ->
        printfn "%s" (renderEnvironmentError e)
        -1
