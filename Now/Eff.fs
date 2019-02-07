module Now.Eff

open Console
open Fs
open Sql

open FSharpPlus.Operators

type EffInstruction<'a, 'b> =
    | RunConsole of Console<'a>
    | RunFs of Fs<'a> * (Fs.Error -> 'b)
    | RunSql of Sql<'a> * (Sql.Error -> 'b)
    | RunResult of Result<'a, 'b>

type Eff<'a, 'b> =
    | Free of EffInstruction<Eff<'a, 'b>, 'b>
    | Pure of 'a

let private mapI f = function
    | RunConsole x -> RunConsole (map f x)
    | RunFs (x, mapError) -> RunFs (map f x, mapError)
    | RunSql (x, mapError) -> RunSql (map f x, mapError)
    | RunResult x -> RunResult (map f x)

let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x

let rec mapError f = function
    | Free (RunConsole x) -> x |> map (mapError f) |> RunConsole |> Free
    | Free (RunFs (x, m)) -> x |> map (mapError f) |> (fun eff -> RunFs (eff, m >> f))|> Free
    | Free (RunSql (x, m)) -> x |> map (mapError f) |> (fun eff -> RunSql (eff, m >> f))|> Free
    | Free (RunResult x) -> x |> Result.mapError f |> map (mapError f) |> RunResult |> Free
    | Pure x -> Pure x

let map f = bind (f >> Pure)

type Eff<'a, 'b> with

    static member Return x = Pure x

    static member (>>=) (x, f) = bind f x

let liftCon x = RunConsole x |> mapI Pure |> Free
let liftFs x = RunFs (x, id) |> mapI Pure |> Free
let liftSql x = RunSql (x, id) |> mapI Pure |> Free
let liftRes x = RunResult x |> mapI Pure |> Free

let rec interpret = function
    | Pure a -> Ok a
    | Free (RunConsole x) -> Console.interpret x |> interpret
    | Free (RunFs (x, mapError)) -> Fs.interpret x |> Result.mapError mapError |> Result.bind interpret
    | Free (RunSql (x, mapError)) -> Sql.interpret x |> Result.mapError mapError |> Result.bind interpret
    | Free (RunResult x) -> x |> Result.bind interpret