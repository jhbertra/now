module Now.CommandLine


(*
    DSL
*)


type Instruction<'a> =
    | ReadLine of (string -> 'a)
    | Write of string * 'a
    | WriteLine of string * 'a


type Program<'a> =
    | Free of Instruction<Program<'a>>
    | Pure of 'a


let private mapI f = function
    | ReadLine next -> ReadLine (next >> f)
    | Write (line, next) -> Write (line, next |> f)
    | WriteLine (line, next) -> WriteLine (line, next |> f)


let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x


let map f = bind (f >> Pure)


type Program<'a> with
    
    static member Return x = Pure x

    static member (>>=) (x, f) = bind f x


(*
    Interpreter
*)


open System


let rec interpret = function
    | Pure a -> a

    | Free (ReadLine next) ->
        Console.ReadLine () |> next |> interpret

    | Free (Write (line, next)) ->
        Console.Write line
        interpret next

    | Free (WriteLine (line, next)) ->
        Console.WriteLine line
        interpret next


(*
    Public API
*)


let readLine = Free (ReadLine Pure)
let write line = Free (Write (line, Pure ()))
let writeLine line = Free (WriteLine (line, Pure ()))