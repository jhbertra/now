open Now.Now
open FSharpPlus.Operators


[<EntryPoint>]
let main argv = 
    match main argv >>= (interpret >> Now.Eff.interpret) with
    | Ok () ->
        0
    | Error e ->
        printfn "%s" (renderError e)
        -1
