module Now.Sql
open FSharpPlus.Builders
open System.Data.SQLite
open System.IO
open FSharpPlus
open FSharpPlus.Data


(*
    Domain
*)


type Database = Database of name : string * location : string
type Parameter = string * obj
type Query = string * Parameter list

let param name value = (name, value :> obj)


(*
    Error
*)


type Error =
    | IoError of IOException
    | SqlError of SQLiteException


(*
    DSL
*)


type Instruction<'a> =
    | RunInTransaction of Database * Sql<unit> * 'a
    | Drop of Database * 'a
    | ExecNonQuery of Database * Query * (int -> 'a)
    | ExecQuery of Database * Query * (SQLiteDataReader -> 'a)


and Sql<'a> =
    | Free of Instruction<Sql<'a>>
    | Pure of 'a


let private mapI f = function
    | RunInTransaction(db, sql, next) -> RunInTransaction(db, sql, next |> f)
    | Drop(db, next) -> Drop(db, next |> f)
    | ExecNonQuery(db, query, next) -> ExecNonQuery(db, query, next >> f)
    | ExecQuery(db, query, next) -> ExecQuery(db, query, next >> f)


let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x


let map f = bind (f >> Pure)


type Sql<'a> with
    
    static member Return x = Pure x

    static member (>>=) (x, f) = bind f x


(*
    Interpreter
*)


open System
open FSharpPlus.Operators


let private getDbFile (Database (name, location)) = Path.Combine (location, sprintf "%s.sqlite" name)


let private getConnection db =
    monad {
        let! connections = StateT.hoist get
        match Map.tryFind db connections with
        | Some (c : SQLiteConnection) ->
            return! lift <| Ok c
        | None ->
            let c = new SQLiteConnection(sprintf "Data Source=%s;Version=3;" (getDbFile db))
            c.Open()
            do! Map.add db c connections |> put |> StateT.hoist
            return! lift <| Ok c
    }


let private prepareCommand (text, parameters) (connection : SQLiteConnection) =
    let command = connection.CreateCommand()
    command.CommandText <- text
    for (name, value) in Seq.ofList parameters do
        command.Parameters.AddWithValue(name, value) |> ignore
    command


let rec private interpret'<'a> : Sql<'a> -> StateT<Map<Database, SQLiteConnection>, Result<'a * Map<Database, SQLiteConnection>, Error>> = function
    | Pure a -> lift (Ok a)

    | Free(RunInTransaction(db, sql, next)) ->
        getConnection db
        >>= ( fun (connection : SQLiteConnection) ->
                use transaction = connection.BeginTransaction()
                try
                    let x = interpret' sql
                    transaction.Commit()
                    x
                with
                | e ->
                    transaction.Rollback()
                    raise e
            )
        |> map (konst next)
        >>= interpret'

    | Free(Drop(db, next)) ->
        let x =
            monad {
                let! connections = StateT.hoist get
                match Map.tryFind db connections with
                | Some (c : SQLiteConnection) ->
                    c.Close()
                    c.Dispose()
                    return! Map.remove db connections |> put |> StateT.hoist
                | None ->
                    return! lift <| Ok ()
            }
            |> map (konst next)
        x >>= interpret'

    | Free(ExecNonQuery(db, query, next)) ->
        getConnection db
        |> map (prepareCommand query)
        |> map (fun (command : SQLiteCommand) -> command.ExecuteNonQuery())
        |> map next
        >>= interpret'

    | Free(ExecQuery(db, query, next)) ->
        getConnection db
        |> map (prepareCommand query)
        |> map (fun (command : SQLiteCommand) -> command.ExecuteReader())
        |> map next
        >>= interpret'


let interpret program =
    try
        monad {
            let! (result, connections) = StateT.run (interpret' program) Map.empty
            for connection : SQLiteConnection in Map.values connections do
                connection.Close()
            return result
        }
    with
    | :? SQLiteException as e -> SqlError e |> Error
    | :? IOException as e -> IoError e |> Error


(*
    Public API
*)


let runInTransaction db sql = RunInTransaction(db, sql, Pure ()) |> Free
let drop db = Drop(db, Pure()) |> Free
let execNonQuery db query = ExecNonQuery(db, query, Pure) |> Free
let execQuery db query = ExecQuery(db, query, Pure) |> Free

let mapReader<'a> f (sql : Sql<SQLiteDataReader>) : Sql<'a list> =
    map
        ( fun (reader : SQLiteDataReader) ->
            let mutable results = []
            while reader.Read() do
                results <- (f reader)::results
            results
        )
        sql

let getOptional<'a> ordinal (reader : SQLiteDataReader) =
    reader.GetValue ordinal
    |> (function
       | :? DBNull as x -> None
       | x -> x :?> 'a |> Some
       )

let groupByOption f =
    List.map (fanout f id)
    >> List.groupBy fst
    >> List.map (fun (x, y) -> (x, List.map snd y))
    >> List.collect (fun (key , x) -> Option.toList key |> List.map ((flip tuple2) x))