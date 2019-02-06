namespace Now

open System.IO
open Fs
open Sql
open Eff
open FSharpPlus.Builders

(*
    Domain
*)

type GlobalEnvironment = {
    globalDir : string
    database : Database
}


type LocalEnvironment = {
    localDir : string
    database : Database
    globalEnv : GlobalEnvironment
}


module Environment =

    (*
        Error
    *)

    type Error =
    | IoError of exn
    | GlobalDirExists
    | GlobalDirMissing
    | LocalDirExists
    | LocalDirMissing


    (*
        DSL
    *)

    type Instruction<'a> =
    | ReadGlobalEnvironment of (GlobalEnvironment -> 'a)
    | ReadLocalEnvironment of (LocalEnvironment -> 'a)
    | InitGlobal of 'a
    | InitLocal of 'a
    | UninstallGlobal of 'a
    | UninstallLocal of 'a

    type Program<'a> =
    | Free of Instruction<Program<'a>>
    | Pure of 'a

    let private mapI f = function
    | ReadGlobalEnvironment next -> ReadGlobalEnvironment(next >> f)
    | ReadLocalEnvironment next -> ReadLocalEnvironment(next >> f)
    | InitGlobal next -> InitGlobal(next |> f)
    | InitLocal next -> InitLocal(next |> f)
    | UninstallGlobal next -> UninstallGlobal(next |> f)
    | UninstallLocal next -> UninstallLocal(next |> f)

    let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x

    let readGlobalEnvironment = Free(ReadGlobalEnvironment Pure)
    let readLocalEnvironment = Free(ReadLocalEnvironment Pure)
    let initGlobal = Free(InitGlobal(Pure()))
    let initLocal = Free(InitLocal(Pure()))
    let uninstallGlobal = Free(UninstallGlobal(Pure()))
    let uninstallLocal = Free(UninstallLocal(Pure()))


    (*
        Interpreter
    *)

    let private getGlobalEnv () =
        monad {
            let! userHome = liftFs home
            let globalDir = Path.Combine(userHome, ".now\\")
            do! liftFs <| requireDir globalDir
            return {
                globalDir = globalDir
                database = Database ("now", globalDir)
            }
        }

    let private getLocalEnv () =
        monad {
            let! globalEnv = getGlobalEnv ()
            let! pwd = liftFs pwd
            let localDir = Path.Combine(pwd, ".now\\")
            do! liftFs <| requireDir localDir

            return {
                globalEnv = globalEnv
                database = Database ("now", localDir)
                localDir = localDir
            }
        }
        
    open FSharpPlus.Operators
    
    let rec interpret = function
    | Pure a -> result a
    | Free(ReadGlobalEnvironment next) -> getGlobalEnv () |> map next >>= interpret
    | Free(ReadLocalEnvironment next) -> getLocalEnv () |> map next >>= interpret
    | Free(InitGlobal next) ->
        monad {
            let! userHome = liftFs home
            let globalDir = Path.Combine(userHome, ".now\\")
            do! liftFs <| mkdir globalDir
            return!
                execNonQuery
                    (Database ("now", globalDir))
                    ( Query
                        ( """
                          CREATE TABLE [MigrationHistory]
                            ( [Id] GUID NOT NULL PRIMARY KEY
                            , [Version] INT NOT NULL UNIQUE
                            , [RunDate] DATETIMEOFFSET NOT NULL
                            )
                          """
                        , []
                        )
                    )
                |> liftSql
        }
        |> map (konst next)
        >>= interpret
    | Free(InitLocal next) ->
        monad {
            let! pwd = liftFs pwd
            let localDir = Path.Combine(pwd, ".now\\")
            do! liftFs <| mkdir localDir
            return!
                execNonQuery
                    (Database ("now", localDir))
                    ( Query
                        ( """
                          CREATE TABLE [MigrationHistory]
                            ( [Id] GUID NOT NULL PRIMARY KEY
                            , [Version] INT NOT NULL UNIQUE
                            , [RunDate] DATETIMEOFFSET NOT NULL
                            )
                          """
                        , []
                        )
                    )
                |> liftSql
        }
        |> map (konst next)
        >>= interpret
    | Free(UninstallGlobal next) ->
        monad {
            let! globalEnv = getGlobalEnv ()
            do! liftSql <| Sql.drop globalEnv.database
            do! liftFs <| rmdir globalEnv.globalDir
        }
        |> map (konst next)
        >>= interpret
    | Free(UninstallLocal next) ->
        monad {
            let! localEnv = getLocalEnv ()
            do! liftSql <| Sql.drop localEnv.database
            do! liftFs <| rmdir localEnv.localDir
        }
        |> map (konst next)
        >>= interpret
