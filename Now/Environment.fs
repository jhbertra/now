namespace Now

open System.IO


(*
    Domain
*)


type Environment = {
    pwd : string
    userHome : string
    globalDir : string option
    localDir : string option
}


module Environment =

    (*
        Error
    *)

    type Error =
    | IoError of exn
    | GlobalDirExists
    | LocalDirExists


    (*
        DSL
    *)
    
    type Instruction<'a> =
    | ReadEnvironment of (Environment -> 'a)
    | InitGlobalDir of 'a
    | InitLocalDir of 'a
    
    type Program<'a> =
    | Free of Instruction<Program<'a>>
    | Pure of 'a
    
    let private mapI f = function
    | ReadEnvironment next -> ReadEnvironment (next >> f)
    | InitGlobalDir next -> InitGlobalDir (next |> f)
    | InitLocalDir next -> InitLocalDir (next |> f)
    
    let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x
    
    type Program<'a> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x
    
    let readEnvironment = Free (ReadEnvironment Pure)
    let initGlobalDir = Free (InitGlobalDir (Pure ()))
    let initLocalDir = Free (InitLocalDir (Pure ()))


    (*
        Interpreter
    *)

    open FSharpPlus.Builders
    open FSharpPlus.Operators
    
    let rec interpret = function
    | Pure a -> Ok a
    | Free (ReadEnvironment next) ->
        monad {
            let! pwd = Result.catch (fun () -> System.Environment.CurrentDirectory) |> Result.mapError IoError
            let! userHome =
                   Result.catch (fun () -> System.Environment.GetFolderPath System.Environment.SpecialFolder.UserProfile)
                   |> Result.mapError IoError
            let! globalDir =
                Result.catch
                    (fun () ->
                        Path.Combine (userHome, ".now")
                        |> Some
                        |> Option.filter Directory.Exists
                    )
               |> Result.mapError IoError
            let! localDir =
                Result.catch
                    (fun () ->
                        Path.Combine (pwd, ".now")
                        |> Some
                        |> Option.filter Directory.Exists
                    )
               |> Result.mapError IoError

            return {
                pwd = pwd
                userHome = userHome
                globalDir = globalDir
                localDir = localDir
            }
        }
        |> Result.map next
        >>= interpret
    | Free (InitGlobalDir next) ->
        monad {
            let! userHome =
               Result.catch (fun () -> System.Environment.GetFolderPath System.Environment.SpecialFolder.UserProfile)
               |> Result.mapError IoError
            let globalDir = Path.Combine (userHome, ".now")
            let! exists = Result.catch (fun () -> Directory.Exists globalDir) |> Result.mapError IoError

            if exists then
                do! Error GlobalDirExists
            else
                do! Result.catch (fun () -> Directory.CreateDirectory globalDir |> ignore) |> Result.mapError IoError
        }
        |> Result.map (konst next)
        >>= interpret
    | Free (InitLocalDir next) ->
        monad {
            let! pwd = Result.catch (fun () -> System.Environment.CurrentDirectory) |> Result.mapError IoError
            let localDir = Path.Combine (pwd, ".now")
            let! exists = Result.catch (fun () -> Directory.Exists localDir) |> Result.mapError IoError

            if exists then
                return! Error LocalDirExists
            else
                return! Result.catch (fun () -> Directory.CreateDirectory localDir |> ignore) |> Result.mapError IoError
        }
        |> Result.map (konst next)
        >>= interpret