namespace Now

open System
open Fs
open Sql
open Eff

(*
    Domain
*)


type PluginPropertyAssignment = {
    id : int
    property : PluginProperty
    value : string
}


module PluginPropertyAssignment =

    let create id property value = { id = id; property = property; value = value }


type PluginAssignment = {
    id : int
    plugin : Plugin
    properties : PluginPropertyAssignment list 
}


module PluginAssignment =

    let create id plugin properties = { id = id; plugin = plugin; properties = properties }


type Task = {
    id : int
    name : string
    plugins : PluginAssignment list
}


module Task =

    let create id name plugins = { id = id; name = name; plugins = plugins }

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
    open System.Data.SQLite
    open FSharpPlus.Builders
    open FSharpPlus.Operators
    
    let liftSql' sql = liftSql sql |> mapError SqlError
    let liftFs' fs = liftFs fs |> mapError FileError
    
    type private TaskRecord = {
        taskName : string
        taskId : int
        taskPluginId : int option
        pluginName : string option
        pluginId : int option
        pluginPropertyName : string option
        pluginPropertyId : int option
        taskPluginPropertyId : int option
        taskPluginPropertyValue : string option
    }

    let groupByOption f =
        List.map (fanout f id)
        >> List.groupBy fst
        >> List.map (fun (x, y) -> (x, List.map snd y))
        >> List.collect (fun (key , x) -> Option.toList key |> List.map ((flip tuple2) x))

    let private getTaskSql =
        """
        SELECT
            [Task].[Name] AS [TaskName]
          , [Task].[Id] AS [TaskId]
          , [TaskPlugin].[Id] AS [TaskPluginId]
          , [Plugin].[Name] AS [PluginName]
          , [Plugin].[Id] AS [PluginId]
          , [PluginProperty].[Name] AS [PluginPropertyName]
          , [PluginProperty].[Id] AS [PluginPropertyId]
          , [TaskPluginProperty].[Id] AS [TaskPluginPropertyId]
          , [TaskPluginProperty].[Value] AS [TaskPluginPropertyValue]
        FROM [Task]
            LEFT JOIN
              ( [TaskPlugin]
                    INNER JOIN [Plugin]
                    ON [Plugin].[Id] = [TaskPlugin].[PluginId]
                
                    LEFT JOIN
                      ( [PluginProperty]
                            INNER JOIN [TaskPluginProperty]
                            ON [TaskPluginProperty].[PluginPropertyId] = [PluginProperty].[Id]
                      )
                    ON [TaskPluginProperty].[TaskPluginId] = [TaskPlugin].[Id]
                    AND [PluginProperty].[PluginId] = [Plugin].[Id]
              )
        ON [TaskPlugin].[TaskId] = [Task].[Id]
        """

    let getOptional<'a> ordinal (reader : SQLiteDataReader) =
        reader.GetValue ordinal
        |> (function
           | :? DBNull as x -> None
           | x -> x :?> 'a |> Some
           )
    
    let private mapTaskReader =
        mapReader
            (fun reader ->
                { taskName = reader.GetString 0
                  taskId = reader.GetInt32 1
                  taskPluginId = reader |> getOptional<Int64> 2 |> Option.map int
                  pluginName = reader |> getOptional<string> 3
                  pluginId = reader |> getOptional<Int64> 4 |> Option.map int
                  pluginPropertyName = reader |> getOptional<string> 5
                  pluginPropertyId = reader |> getOptional<Int64> 6 |> Option.map int
                  taskPluginPropertyId = reader |> getOptional<Int64> 7 |> Option.map int
                  taskPluginPropertyValue = reader |> getOptional<string> 8 })

    let private buildTasks =
        List.groupBy (fun x -> (x.taskId, x.taskName))
        >> map
            (fun ((taskId, taskName), taskg) ->
                create
                    taskId
                    taskName
                    (taskg
                    |> groupByOption (fun x -> tuple3 <!> x.taskPluginId <*> x.pluginId <*> x.pluginName)
                    |> map
                        (fun ((taskPluginId, pluginId, pluginName), taskPluginG) ->
                            PluginAssignment.create
                                taskPluginId
                                (Plugin.create
                                    pluginId
                                    pluginName
                                    (taskPluginG
                                    |> List.collect
                                        (fun x ->
                                            (tuple2
                                                <!> x.pluginPropertyName
                                                <*> x.pluginPropertyId
                                            )
                                            |> Option.toList)
                                    |> map
                                        (fun (pluginPropertyName, pluginPropertyId) ->
                                            PluginProperty.create pluginPropertyId pluginPropertyName
                                        )
                                    )
                                )
                                (taskPluginG
                                |> List.collect
                                    (fun x ->
                                        (tuple4
                                            <!> x.pluginPropertyName
                                            <*> x.pluginPropertyId
                                            <*> x.taskPluginPropertyValue
                                            <*> x.taskPluginPropertyId
                                        )
                                        |> Option.toList)
                                |> map
                                    (fun (pluginPropertyName, pluginPropertyId, taskPluginPropertyValue, taskPluginPropertyId) ->
                                        PluginPropertyAssignment.create
                                            taskPluginPropertyId
                                            (PluginProperty.create pluginPropertyId pluginPropertyName)
                                            taskPluginPropertyValue
                                    )
                                )
                        )
                    )
            )

    let private getTask' db name =
        execQuery
            db
            ( Query
                ( getTaskSql ++ "WHERE [Task].[Name] = @name"
                , [ param "@name" name ]
                )
            )
        |> mapTaskReader
        |> map (buildTasks >> tryHead)
        |> liftSql'
    
    let private getTaskByIdSql db id =
        execQuery
            db
            ( Query
                ( getTaskSql ++ "WHERE [Task].[Id] = @id"
                , [ param "@id" id ]
                )
            )
        |> mapTaskReader
        |> map (buildTasks >> tryHead)
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
                let! task = getTask' env.database name
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
                let! task = getTask' env.database name
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
                let! task = getTask' env.database name
                match task with
                | Some t -> return t
                | None -> return! TaskNotFound name |> Error |> liftRes
            }
            |> map next
            >>= interpret
    
        | Free(GetTasks(env, next)) ->
            execQuery env.database ( Query ( getTaskSql, []) )
            |> mapTaskReader
            |> map buildTasks
            |> liftSql'
            |> map next
            >>= interpret
    
        | Free(RenameTask(env, oldName, newName, next)) ->
            monad {
                let! oldTask = getTask' env.database oldName
                let! newTask = getTask' env.database newName

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
