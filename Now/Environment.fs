namespace Now

open System.IO
open Fs
open Sql
open Eff
open FSharpPlus.Builders

(*
    Domain
*)

type Env = {
    rootDir : string
    pluginDir : string
    database : Database
}


module Env =

    (*
        Error
    *)

    type Error =
        | FileError of Fs.Error
        | SqlError of Sql.Error
        | RootDirExists
        | RootDirMissing


    (*
        DSL
    *)

    type Instruction<'a> =
        | ReadEnv of (Env -> 'a)
        | Install of 'a
        | Uninstall of 'a

    type Program<'a> =
        | Free of Instruction<Program<'a>>
        | Pure of 'a

    let private mapI f = function
        | ReadEnv next -> ReadEnv(next >> f)
        | Install next -> Install(next |> f)
        | Uninstall next -> Uninstall(next |> f)

    let rec bind f = function
        | Free x -> x |> mapI (bind f) |> Free
        | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a> with

        static member Return x = Pure x

        static member (>>=) (x, f) = bind f x

    let readEnv = Free(ReadEnv Pure)
    let install = Free(Install(Pure()))
    let uninstall = Free(Uninstall(Pure()))


    (*
        Interpreter
    *)
        
    open FSharpPlus.Operators

    let private getEnv () =
        monad {
            let! userHome = liftFs home |> mapError FileError
            let rootDir = Path.Combine(userHome, ".now\\")
            do! (liftFs <| requireDir rootDir) |> mapError (konst RootDirMissing)
            return {
                rootDir = rootDir
                pluginDir = Path.Combine(rootDir, "plugins\\")
                database = Database ("now", rootDir)
            }
        }
    
    let rec interpret = function
        | Pure a -> result a

        | Free(ReadEnv next) -> getEnv () |> map next >>= interpret

        | Free(Install next) ->
            monad {
                let! userHome = liftFs home |> mapError FileError
                let rootDir = Path.Combine(userHome, ".now\\")
                do! (liftFs <| mkdir rootDir) |> mapError (konst RootDirExists)
                return!
                    execNonQuery
                        (Database ("now", rootDir))
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
                    |> mapError SqlError
            }
            |> map (konst next)
            >>= interpret

        | Free(Uninstall next) ->
            monad {
                let! env = getEnv ()
                do! (liftFs <| rmdir env.rootDir) |> mapError (konst RootDirMissing)
            }
            |> map (konst next)
            >>= interpret
