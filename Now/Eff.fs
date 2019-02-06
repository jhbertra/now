namespace Now


module Eff =

    type Error = FileError of Fs.Error

    type EffT<'a> = Run of CommandLine.Program<Fs.Program<Result<'a, Error>>>

    type Program<'a> =
        | Free of EffT<Program<'a>>
        | Pure of 'a

    let mapStack f = CommandLine.map (Fs.map (Result.map f))
    let mapT f (Run p) = mapStack f p |> Run

    let rec bind f = function
        | Free x -> x |> mapT (bind f) |> Free
        | Pure x -> f x

    type Program<'a> with

        static member Return x = Pure x

        static member (>>=) (x, f) = bind f x

    let wrap x = x |> Run |> mapT Pure |> Free
    let liftFS x = wrap <| CommandLine.Pure (Fs.map Ok x)
    let liftRes x = wrap <| CommandLine.Pure (Fs.Pure x)

    open FSharpPlus.Operators

    let rec interpret = function
        | Pure a -> Ok a
        | Free(Run p) ->
            p
            |> CommandLine.interpret
            |> (Fs.interpret >> Result.mapError FileError)
            >>= id
            >>= interpret