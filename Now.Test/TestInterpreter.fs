module Now.Test.TestInterpreter
open Now

type ConsoleLog =
    | ConExec of string * string list * int
    | ConReadLine of string
    | ConWrite of string
    | ConWriteLine of string

type Failrue<'a> =
    | UnrunConsoleLogs of ConsoleLog list
    | UnexpectedConsoleLog of ConsoleLog option * Now.Console.Console<'a>
    | UnexpectedProgramError of Now.Now.Error

open Now.Console
open FSharpPlus.Operators

let rec interpretConsole expected con =
    match (con, expected) with
    | (Pure a, _) -> Ok (expected, a)

    | (Free (Exec (cmd, args, next)), (ConExec (eCmd, eArgs, result)) :: expected)
        when cmd = eCmd && args = eArgs -> next result |> interpretConsole expected

    | (Free (ReadLine next), (ConReadLine result) :: expected) ->
        next result |> interpretConsole expected

    | (Free (Write (line, next)), (ConWrite eLine) :: expected)
        when line = eLine -> next |> interpretConsole expected

    | (Free (WriteLine (line, next)), (ConWriteLine eLine) :: expected)
        when line = eLine -> next |> interpretConsole expected
    
    | (con, expected) -> Error (UnexpectedConsoleLog (List.tryHead expected, con))

open Now.Fs

let rec interpretFs home fs =
    match fs with
    | Free (Home next) -> interpretFs home (next home)
    | fs -> Fs.interpret fs

open Now.Eff

let rec interpret home consoleLogs eff =
    match (eff, consoleLogs) with
    | (Pure a, []) -> Ok (Ok a)
    | (Pure a, x) -> Error (UnrunConsoleLogs x)
    | (Free (RunConsole x), consoleLogs) -> interpretConsole consoleLogs x >>= uncurry (interpret home)
    | (Free (RunFs (x, mapError)), _) ->
        match interpretFs home x with
        | Ok x -> interpret home consoleLogs x
        | Error e -> Ok (Error (mapError e))
    | (Free (RunSql (x, mapError)), _) ->
        match Sql.interpret x with
        | Ok x -> interpret home consoleLogs x
        | Error e -> Ok (Error (mapError e))
    | (Free (RunResult r), _) -> 
        match r with
        | Ok x -> interpret home consoleLogs x
        | Error e -> Ok (Error e)
