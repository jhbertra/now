module Now.Result


open FSharpPlus.Operators


let catch f =
    try
        f () |> Ok
    with
    | e -> Error e


let inline (<*|+>) mfab ma =
    match (mfab, ma) with
    | (Error e1, Error e2) -> Error (e1 ++ e2)
    | (Error e, _) -> Error e
    | (_, Error e) -> Error e
    | (Ok fab, Ok a) -> fab a |> Ok