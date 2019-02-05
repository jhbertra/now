namespace Now

(*
    Domain
*)


type Task = {
    id : int
    name : string
 }


module Task =


    (*
        DSL
    *)

    type Instruction<'a> =
    | CreateTask of LocalEnvironment * string * 'a
    | DeleteTask of LocalEnvironment * string * 'a
    | GetTask of LocalEnvironment * string * (Task option -> 'a)
    | GetTasks of LocalEnvironment * (Task list -> 'a)
    | RenameTask of LocalEnvironment * string * string * 'a

    type Program<'a> =
    | Free of Instruction<Program<'a>>
    | Pure of 'a

    let private mapI f = function
    | CreateTask(env, name, next) -> CreateTask(env, name, next |> f)
    | DeleteTask(env, name, next) -> DeleteTask(env, name, next |> f)
    | GetTask(env, name, next) -> GetTask(env, name, next >> f)
    | GetTasks(env, next) -> GetTasks(env, next >> f)
    | RenameTask(env, oldName, newName , next) -> RenameTask(env, oldName, newName , next |> f)

    let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x

    let createTask env name = Free(CreateTask(env, name, Pure()))
    let deleteTask env name = Free(DeleteTask(env, name, Pure()))
    let getTask env name = Free(GetTask(env, name, Pure))
    let getTasks env = Free(GetTasks(env, Pure))
    let renameTask env oldName newName = Free(RenameTask(env, oldName, newName, Pure()))


    (*
        Interpreter
    *)

    open FSharpPlus.Operators

    let rec interpret = function
    | Pure a -> Ok a

    | Free(CreateTask(env, name, next)) ->
        Data.buildDatabaseFile env.localDir
        |> Data.executeLocalNonQuery
            """
            INSERT INTO [Task] ([Name])
            VALUES (@name)
            """
            [ ("@name", name) ]
        |> map (konst next)
        >>= interpret

    | Free(DeleteTask(env, name, next)) ->
        Data.buildDatabaseFile env.localDir
        |> Data.executeLocalNonQuery
            """
            DELETE FROM [Task]
            WHERE [Name] = @name
            """
            [ ("@name", name) ]
        |> map (konst next)
        >>= interpret

    | Free(GetTask(env, name, next)) ->
        Data.buildDatabaseFile env.localDir
        |> Data.executeLocalQuery
            """
            SELECT
                [Name]
              , [Id]
            FROM [Task]
            WHERE [Name] = @name
            LIMIT 1
            """
            (fun reader -> { name = reader.GetString 0; id = reader.GetInt32 1 })
            [
                ("@name", name)
            ]
        |> map tryHead
        |> map next
        >>= interpret

    | Free(GetTasks(env, next)) ->
        Data.buildDatabaseFile env.localDir
        |> Data.executeLocalQuery
            """
            SELECT
                [Name]
              , [Id]
            FROM [Task]
            ORDER BY [Name]
            """
            (fun reader -> { name = reader.GetString 0; id = reader.GetInt32 1 })
            []
        |> map next
        >>= interpret

    | Free(RenameTask(env, oldName, newName, next)) ->
        Data.buildDatabaseFile env.localDir
        |> Data.executeLocalNonQuery
            """
            UPDATE [Task] SET    
                [Name] = @newName
            WHERE [Name] = @oldName
            """
            [
                ("@oldName", oldName)
                ("@newName", newName)
            ]
        |> map (konst next)
        >>= interpret
