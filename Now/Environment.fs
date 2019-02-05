namespace Now

(*
    Domain
*)


type GlobalEnvironment = {
    globalDir : string
 }


type LocalEnvironment = {
    localDir : string
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
    | InitGlobalDir of 'a
    | InitLocalDir of 'a
    | UninstallGlobalDir of 'a
    | UninstallLocalDir of 'a

    type Program<'a> =
    | Free of Instruction<Program<'a>>
    | Pure of 'a

    let private mapI f = function
    | ReadGlobalEnvironment next -> ReadGlobalEnvironment(next >> f)
    | ReadLocalEnvironment next -> ReadLocalEnvironment(next >> f)
    | InitGlobalDir next -> InitGlobalDir(next |> f)
    | InitLocalDir next -> InitLocalDir(next |> f)
    | UninstallGlobalDir next -> UninstallGlobalDir(next |> f)
    | UninstallLocalDir next -> UninstallLocalDir(next |> f)

    let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x

    let map f = bind (f >> Pure)

    type Program<'a> with
        static member Return x = Pure x
        static member (>>=) (x, f) = bind f x

    let readGlobalEnvironment = Free(ReadGlobalEnvironment Pure)
    let readLocalEnvironment = Free(ReadLocalEnvironment Pure)
    let initGlobalDir = Free(InitGlobalDir(Pure()))
    let initLocalDir = Free(InitLocalDir(Pure()))
    let uninstallGlobalDir = Free(UninstallGlobalDir(Pure()))
    let uninstallLocalDir = Free(UninstallLocalDir(Pure()))


    (*
        Interpreter
    *)

    open System.IO
    open FSharpPlus.Builders
    open FSharpPlus.Operators

    let private getGlobalEnv() =
        monad {
            let! userHome =
                   Result.catch (fun () -> System.Environment.GetFolderPath System.Environment.SpecialFolder.UserProfile)
                   |> Result.mapError IoError

            let! globalDir =
                Result.catch
                    (fun () ->
                        let dir = Path.Combine(userHome, ".now\\")
                        (dir, Directory.Exists dir)
                    )
               |> Result.mapError IoError
               |> Result.filter snd (GlobalDirMissing)
               |> Result.map fst

            return {
                globalDir = globalDir
            }
        }

    let rec interpret = function
    | Pure a -> Ok a
    | Free(ReadGlobalEnvironment next) -> getGlobalEnv() |> Result.map next >>= interpret
    | Free(ReadLocalEnvironment next) ->
        monad {
            let! globalEnv = getGlobalEnv()

            let! pwd = Result.catch (fun () -> System.Environment.CurrentDirectory) |> Result.mapError IoError

            let! localDir =
                Result.catch
                    (fun () ->
                        let dir = Path.Combine(pwd, ".now\\")
                        (dir, Directory.Exists dir)
                    )
               |> Result.mapError IoError
               |> Result.filter snd (LocalDirMissing)
               |> Result.map fst

            return {
                globalEnv = globalEnv
                localDir = localDir
            }
        }
        |> Result.map next
        >>= interpret
    | Free(InitGlobalDir next) ->
        monad {
            let! userHome =
               Result.catch (fun () -> System.Environment.GetFolderPath System.Environment.SpecialFolder.UserProfile)
               |> Result.mapError IoError
            let globalDir = Path.Combine(userHome, ".now\\")
            let! exists = Result.catch (fun () -> Directory.Exists globalDir) |> Result.mapError IoError

            if exists then
                do! Error GlobalDirExists
            else
                do! Result.catch (fun () -> Directory.CreateDirectory globalDir |> ignore) |> Result.mapError IoError
        }
        |> Result.map (konst next)
        >>= interpret
    | Free(InitLocalDir next) ->
        monad {
            let! pwd = Result.catch (fun () -> System.Environment.CurrentDirectory) |> Result.mapError IoError
            let localDir = Path.Combine(pwd, ".now\\")
            let! exists = Result.catch (fun () -> Directory.Exists localDir) |> Result.mapError IoError

            if exists then
                return! Error LocalDirExists
            else
                return! Result.catch (fun () -> Directory.CreateDirectory localDir |> ignore) |> Result.mapError IoError
        }
        |> Result.map (konst next)
        >>= interpret
    | Free(UninstallGlobalDir next) ->
        monad {
            let! userHome =
               Result.catch (fun () -> System.Environment.GetFolderPath System.Environment.SpecialFolder.UserProfile)
               |> Result.mapError IoError
            let globalDir = Path.Combine(userHome, ".now\\")
            let! exists = Result.catch (fun () -> Directory.Exists globalDir) |> Result.mapError IoError

            if exists then
                do! Result.catch (fun () -> Directory.Delete(globalDir, true)) |> Result.mapError IoError
            else
                return ()
        }
        |> Result.map (konst next)
        >>= interpret
    | Free(UninstallLocalDir next) ->
        monad {
            let! pwd = Result.catch (fun () -> System.Environment.CurrentDirectory) |> Result.mapError IoError
            let localDir = Path.Combine(pwd, ".now\\")
            let! exists = Result.catch (fun () -> Directory.Exists localDir) |> Result.mapError IoError

            if exists then
                do! Result.catch (fun () -> Directory.Delete(localDir, true)) |> Result.mapError IoError
            else
                return ()
        }
        |> Result.map (konst next)
        >>= interpret
