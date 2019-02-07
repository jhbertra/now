namespace Now

open System
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
    
    (*
        Error
    *)

    type Error =
    | SqlError of Sql.Error
    | TaskExists of string
    | TaskNotFound of string

    (*
        DSL
    *)

    type Instruction<'a> =
        | CreateTask of LocalEnvironment * string * 'a
        | DeleteTask of LocalEnvironment * string * 'a
        | GetTask of LocalEnvironment * string * (Task -> 'a)
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
    
//    let getActiveTask env =    
//        monad {
//            let! taskActions = getTaskActions env
//        }


    (*
        Interpreter
    *)
    open FSharpPlus.Builders
    open FSharpPlus.Operators
    
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
        |> liftSql
        |> map tryHead
        |> mapError SqlError

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
                        |> liftSql
                        |> mapError SqlError
            }
            |> map (konst next)
            >>= interpret
    
        | Free(DeleteTask(env, name, next)) ->
            monad {
                let! task = getTaskSql env.database name
                if Option.isNone task then
                    return! TaskNotFound name |> Error |> liftRes
                else
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
                        |> liftSql
                        |> mapError SqlError
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
            |> liftSql
            |> mapError SqlError
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
                        |> liftSql
                        |> mapError SqlError
            }
            |> map (konst next)
            >>= interpret
