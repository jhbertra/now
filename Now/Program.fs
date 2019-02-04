open FSharpPlus.Builders
open Now.Environment


let renderEnvironmentError = function
| IoError exn -> sprintf "IO Error: %s" exn.Message
| GlobalDirExists -> "The global Now directory already exists"
| LocalDirExists -> "A local Now directory already exists"


[<EntryPoint>]
let main argv =
    monad {
        do! initLocalDir
        let! environment = readEnvironment
        printfn "%A" environment
    }
    |> interpret
    |> (function    
        | Ok () -> 0
        | Error e ->
            printfn "%s" (renderEnvironmentError e)
            -1
       )
