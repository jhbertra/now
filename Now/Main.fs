open Now.Now
open FSharpPlus.Operators


[<EntryPoint>]
let main argv =    
    let result =
        parseCommand (List.ofArray argv)
        |> Result.map runCommand
        >>= (interpret >> Now.Eff.interpret)
    match result with
    | Ok () ->
        0
    | Error e ->
        printfn "%s" (renderError e)
        -1
