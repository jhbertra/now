namespace Now

open System
open Fs
open Sql
open Eff

(*
    Domain
*)


type PluginProperty = {
    id : int
    name : string
}


module PluginProperty =

    let create id name = { id = id; name = name }


type Plugin = {
    id : int
    name : string
    properties : PluginProperty list
}


module Plugin =

    let create id name properties = { id = id; name = name; properties = properties }

    (*
        Error
    *)

    type Error =
        | SqlError of Sql.Error
        | FileError of Fs.Error
        | PluginExists of string
        | PluginNotFound of string

    (*
        DSL
    *)

    type Instruction<'a> =
        | InstallPlugin of Env * string * 'a
        | GetPlugin of Env * string * (Plugin -> 'a)
        | GetPlugins of Env * (Plugin list -> 'a)

    type Program<'a> =
        | Free of Instruction<Program<'a>>
        | Pure of 'a

    let private mapI f = function
        | InstallPlugin(env, dir, next) -> InstallPlugin(env, dir, next |> f)
        | GetPlugin(env, name, next) -> GetPlugin(env, name, next >> f)
        | GetPlugins(env, next) -> GetPlugins(env, next >> f)

    let rec bind f = function
        | Free x -> x |> mapI (bind f) |> Free
        | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a> with

        static member Return x = Pure x

        static member (>>=) (x, f) = bind f x

    let installPlugin env dir = Free(InstallPlugin(env, dir, Pure()))
    let getPlugin env name = Free(GetPlugin(env, name, Pure))
    let getPlugins env = Free(GetPlugins(env, Pure))


    (*
        Interpreter
    *)
    open FSharpPlus.Builders
    open FSharpPlus.Operators
    
    let liftSql' sql = liftSql sql |> mapError SqlError
    let liftFs' fs = liftFs fs |> mapError FileError
    
    type private PluginRecord = {
        pluginName : string
        pluginId : int
        pluginPropertyName : string option
        pluginPropertyId : int option
    }

    let private getPluginSql =
        """
        SELECT
            [Plugin].[Name] AS [PluginName]
          , [Plugin].[Id] AS [PluginId]
          , [PluginProperty].[Name] AS [PluginPropertyName]
          , [PluginProperty].[Id] AS [PluginPropertyId]
        FROM [Plugin]
            LEFT JOIN [PluginProperty]
            ON [PluginProperty].[PluginId] = [Plugin].[Id]
        """
    
    let private mapPluginReader =
        mapReader
            (fun reader ->
                { pluginName = reader.GetString 0
                  pluginId = reader.GetInt32 1
                  pluginPropertyName = reader |> getOptional<string> 2
                  pluginPropertyId = reader |> getOptional<Int64> 3 |> Option.map int })

    let private buildPlugins records =
        records
        |> List.groupBy (fun x -> (x.pluginId, x.pluginName))
        |> map
            ( fun ((pluginId, pluginName), pluginG) ->
                create
                    pluginId
                    pluginName
                    ( pluginG
                      |> List.collect (fun x -> tuple2 <!> x.pluginPropertyId <*> x.pluginPropertyName |> Option.toList)
                      |> map (uncurry PluginProperty.create)
                    )
            )

    let private getPlugin' db name =
        execQuery
            db
            ( Query
                ( getPluginSql ++ "WHERE [Plugin].[Name] = @name"
                , [ param "@name" name ]
                )
            )
        |> mapPluginReader
        |> map (buildPlugins >> tryHead)
        |> liftSql'
    
    let private getPluginByIdSql db id =
        execQuery
            db
            ( Query
                ( getPluginSql ++ "WHERE [Plugin].[Id] = @id"
                , [ param "@id" id ]
                )
            )
        |> mapPluginReader
        |> map (buildPlugins >> tryHead)
        |> liftSql'

    let rec interpret = function
        | Pure a -> result a
    
//        | Free(InstallPlugin(env, dir, next)) ->
//            monad {
//                let! task = getTask' env.database name
//                if Option.isSome task then
//                    return! TaskExists name |> Error |> liftRes
//                else
//                    return!
//                        execNonQuery
//                            env.database
//                            ( Query
//                                ( """
//                                  INSERT INTO [Task] ([Name])
//                                  VALUES (@name)
//                                  """
//                                , [ param "@name" name ]
//                                )
//                            )
//                        |> liftSql'
//            }
//            |> map (konst next)
//            >>= interpret
    
        | Free(GetPlugin(env, name, next)) ->
            monad {
                let! task = getPlugin' env.database name
                match task with
                | Some t -> return t
                | None -> return! PluginNotFound name |> Error |> liftRes
            }
            |> map next
            >>= interpret
    
        | Free(GetPlugins(env, next)) ->
            execQuery env.database ( Query ( getPluginSql, []) )
            |> mapPluginReader
            |> map buildPlugins
            |> liftSql'
            |> map next
            >>= interpret
