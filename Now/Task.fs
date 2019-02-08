namespace Now

open System
open Fs
open Sql
open Eff

(*
    Domain
*)


type Action =
| Start of DateTimeOffset
| Stop of DateTimeOffset


type Task = {
    id : int
    name : string
}


module Task =

    let create id name = { id = id; name = name }

    (*
        Error
    *)

    type Error =
        | SqlError of Sql.Error
        | FileError of Fs.Error
        | TaskExists of string
        | TaskNotFound of string
        | ActiveTaskIdNotFound of string
        | ActiveTaskExists
        | ActiveTaskNotFound
        | DeleteActiveTask of string

    (*
        DSL
    *)

    type Instruction<'a> =
        | CreateTask of Env * string * 'a
        | DeleteTask of Env * string * 'a
        | GetActiveTask of Env * (Task option -> 'a)
        | SetActiveTask of Env * Task option * 'a
        | GetTask of Env * string * (Task -> 'a)
        | GetTasks of Env * (Task list -> 'a)
        | RenameTask of Env * string * string * 'a

    type Program<'a> =
        | Free of Instruction<Program<'a>>
        | Pure of 'a

    let private mapI f = function
        | CreateTask(env, name, next) -> CreateTask(env, name, next |> f)
        | DeleteTask(env, name, next) -> DeleteTask(env, name, next |> f)
        | GetActiveTask(env, next) -> GetActiveTask(env, next >> f)
        | GetTask(env, name, next) -> GetTask(env, name, next >> f)
        | GetTasks(env, next) -> GetTasks(env, next >> f)
        | RenameTask(env, oldName, newName , next) -> RenameTask(env, oldName, newName , next |> f)
        | SetActiveTask(env, task, next) -> SetActiveTask(env, task, next |> f)

    let rec bind f = function
        | Free x -> x |> mapI (bind f) |> Free
        | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a> with

        static member Return x = Pure x

        static member (>>=) (x, f) = bind f x

    let createTask env name = Free(CreateTask(env, name, Pure()))
    let deleteTask env name = Free(DeleteTask(env, name, Pure()))
    let getActiveTask env = Free(GetActiveTask(env, Pure))
    let setActiveTask env task = Free(SetActiveTask(env, task, Pure()))
    let getTask env name = Free(GetTask(env, name, Pure))
    let getTasks env = Free(GetTasks(env, Pure))
    let renameTask env oldName newName = Free(RenameTask(env, oldName, newName, Pure()))
    
//    let getActiveTask env =    
//        monad {
//            let! taskActions = getTaskActions env
//        }


    (*
        Interpreter
    *)
    open FSharpPlus.Builders
    open FSharpPlus.Operators
    
    let liftSql' sql = liftSql sql |> mapError SqlError
    let liftFs' fs = liftFs fs |> mapError FileError
    
    let private getTaskSql db name =
        execQuery
            db
            ( Query
                ( """
                  SELECT
                      [Task].[Name]
                    , [Task].[Id]
                  FROM [Task]
                  WHERE [Task].[Name] = @name
                  LIMIT 1
                  """
                , [ param "@name" name ]
                )
            )
        |> mapReader (fun reader -> { name = reader.GetString 0; id = reader.GetInt32 1 })
        |> map tryHead
        |> liftSql'
    
    let private getTaskByIdSql db id =
        execQuery
            db
            ( Query
                ( """
                  SELECT
                      [Task].[Name]
                    , [Task].[Id]
                  FROM [Task]
                  WHERE [Task].[Id] = @id
                  LIMIT 1
                  """
                , [ param "@id" id ]
                )
            )
        |> mapReader (fun reader -> { name = reader.GetString 0; id = reader.GetInt32 1 })
        |> map tryHead
        |> liftSql'

    let getActiveTaskFile { rootDir = rootDir } = IO.Path.Combine(rootDir, ".activetask")

    let getActiveTaskImpl env =
        monad {
            let! fileExists = liftFs' <| Fs.exists (getActiveTaskFile env)
            if fileExists then
                let! taskfile = liftFs' <| read (getActiveTaskFile env)
                let! task = getTaskByIdSql env.database (Int32.Parse taskfile)
                match task with
                | Some t -> return Some t
                | None -> return! ActiveTaskIdNotFound taskfile |> Error |> liftRes
            else
                return None
        }

    let rec interpret = function
        | Pure a -> result a
    
        | Free(CreateTask(env, name, next)) ->
            monad {
                let! task = getTaskSql env.database name
                if Option.isSome task then
                    return! TaskExists name |> Error |> liftRes
                else
                    return!
                        execNonQuery
                            env.database
                            ( Query
                                ( """
                                  INSERT INTO [Task] ([Name])
                                  VALUES (@name)
                                  """
                                , [ param "@name" name ]
                                )
                            )
                        |> liftSql'
            }
            |> map (konst next)
            >>= interpret
    
        | Free(DeleteTask(env, name, next)) ->
            monad {
                let! task = getTaskSql env.database name
                let! activeTask = getActiveTaskImpl env
                match (task, activeTask) with
                | Some t, Some active when t.id = active.id ->
                    return! DeleteActiveTask t.name |> Error |> liftRes
                | None, _->
                    return! TaskNotFound name |> Error |> liftRes
                | Some t, _ ->
                    return!
                        execNonQuery
                            env.database
                            ( Query
                                ( """
                                  DELETE FROM [Task]
                                  WHERE [Name] = @name
                                  """
                                , [ param "@name" name ]
                                )
                            )
                        |> liftSql'
            }
            |> map (konst next)
            >>= interpret
    
        | Free(GetActiveTask(env, next)) -> getActiveTaskImpl env |> map next >>= interpret
    
        | Free(SetActiveTask(env, Some task, next)) ->
            monad {
                let file = getActiveTaskFile env
                let! fileExists = liftFs' <| Fs.exists file
                if fileExists then
                    return! liftRes <| Error ActiveTaskExists
                else
                    do! liftFs' <| mkfile file
                    do! liftFs' <| write file (sprintf "%d" task.id)
            }
            |> map (konst next)
            >>= interpret
    
        | Free(SetActiveTask(env, None, next)) ->
            monad {
                let file = getActiveTaskFile env
                let! fileExists = liftFs' <| Fs.exists file
                if fileExists then
                    do! liftFs' <| rmfile file
                else
                    return! liftRes <| Error ActiveTaskNotFound
            }
            |> map (konst next)
            >>= interpret
    
        | Free(GetTask(env, name, next)) ->
            monad {
                let! task = getTaskSql env.database name
                match task with
                | Some t -> return t
                | None -> return! TaskNotFound name |> Error |> liftRes
            }
            |> map next
            >>= interpret
    
        | Free(GetTasks(env, next)) ->
            execQuery
                env.database
                ( Query
                    ( """
                      SELECT
                          [Name]
                        , [Id]
                      FROM [Task]
                      ORDER BY [Name]
                      """
                    , []
                    )
                )
            |> mapReader (fun reader -> { name = reader.GetString 0; id = reader.GetInt32 1 })
            |> liftSql'
            |> map next
            >>= interpret
    
        | Free(RenameTask(env, oldName, newName, next)) ->
            monad {
                let! oldTask = getTaskSql env.database oldName
                let! newTask = getTaskSql env.database newName

                if Option.isNone oldTask then
                    return! TaskNotFound oldName |> Error |> liftRes
                elif Option.isSome newTask then
                    return! TaskExists newName |> Error |> liftRes
                else
                    return!
                        execNonQuery
                            env.database
                            ( Query
                                ( """
                                  UPDATE [Task] SET    
                                      [Name] = @newName
                                  WHERE [Name] = @oldName
                                  """
                                , [
                                      param "@oldName" oldName
                                      param "@newName" newName
                                  ]
                                )
                            )
                        |> liftSql'
            }
            |> map (konst next)
            >>= interpret
