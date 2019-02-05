open Now.Now
open FSharpPlus.Operators


[<EntryPoint>]
let main argv =    
    let result = parseCommand (List.ofArray argv) |> Result.map runCommand >>= interpret
    match result with
    | Ok () ->
        0
    | Error e ->
        printfn "%s" (renderEnvironmentError e)
        -1
