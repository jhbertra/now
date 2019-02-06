module Now.Eff

open Console
open Fs
open Sql


type Error<'a> =
    | FileError of Fs.Error
    | SqlError of Sql.Error
    | UserError of 'a

type EffT<'a, 'b> = Run of Console<Fs<Sql<Result<'a, Error<'b>>>>>

type Eff<'a, 'b> =
    | Free of EffT<Eff<'a, 'b>, 'b>
    | Pure of 'a

let mapStack f = Console.map (Fs.map (Sql.map (Result.map f)))
let mapT f (Run p) = mapStack f p |> Run

let rec bind f = function
    | Free x -> x |> mapT (bind f) |> Free
    | Pure x -> f x

let rec mapError f = function
    | Free (Run x) ->
        x
        |> Console.map
            ( Fs.map
                ( Sql.map
                    ( Result.mapError
                        ( function
                            | UserError e -> f e |> UserError
                            | SqlError x -> SqlError x
                            | FileError x -> FileError x
                        )
                      >> Result.map (mapError f)
                    )
                )
            )
        |> Run
        |> Free
    | Pure x -> Pure x

type Eff<'a, 'b> with

    static member Return x = Pure x

    static member (>>=) (x, f) = bind f x

let wrap x = x |> Run |> mapT Pure |> Free
let liftCon x = wrap <| Console.map (Fs.Pure << Sql.Pure << Ok) x
let liftFs x = wrap <| Console.Pure (Fs.map (Sql.Pure << Ok) x)
let liftSql x = wrap <| Console.Pure (Fs.Pure (Sql.map Ok x))
let liftRes x = wrap <| Console.Pure (Fs.Pure (Sql.Pure (Result.mapError UserError x)))

open FSharpPlus.Operators

let rec interpret = function
    | Pure a -> Ok a
    | Free(Run p) ->
        p
        |> Console.interpret
        |> (Fs.interpret >> Result.mapError FileError)
        >>= (Sql.interpret >> Result.mapError SqlError)
        >>= id
        >>= interpret