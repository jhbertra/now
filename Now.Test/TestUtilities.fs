namespace Now.Test

open FSharpPlus.Builders

open NUnit.Framework

open System.IO
open Now.Now
open TestInterpreter


type TestResult = {
    databaseExists : bool
    rootDirExists : bool
}


module TestResult =
    
    let empty = {
        databaseExists = false
        rootDirExists = false
    }


module TestUtilities =

    let runSetup args logs =
        let result =
            monad {
                let! program = main args |> Result.mapError UnexpectedProgramError
                let eff = Now.Now.interpret program
                return! interpret "." logs eff
            }

        match result with
        | Ok (Error e) -> Assert.Fail(sprintf "Program failed: %A" e)
        | Error e -> Assert.Fail(sprintf "Unexpected result: %A" e)
        | _ -> ()
        

    let runTest args logs testResult =
        try
            let result =
                monad {
                    let! program = main args |> Result.mapError UnexpectedProgramError
                    let eff = Now.Now.interpret program
                    return! interpret "." logs eff
                }
    
            match (result, testResult) with
            | (Ok (Error actual), Error (expected : Now.Now.Error)) -> Assert.AreEqual(expected, actual)
            | (Ok (Error e), _) -> Assert.Fail(sprintf "Program failed: %A" e)
            | (Ok (Ok _), Error _) -> Assert.Fail(sprintf "Program was expected to fail")
            | (Ok (Ok _), Ok state) ->
                let dbfile = Path.Combine (".now", "now.sqlite")
                if state.databaseExists then
                    Assert.IsTrue(File.Exists dbfile, "Database was expected to exist")
                else
                    Assert.IsFalse(File.Exists dbfile, "Database was not expected to exist")
                if state.rootDirExists then
                    Assert.IsTrue(Directory.Exists ".\\.now", "Root directory was expected to exist")
                else
                    Assert.IsFalse(Directory.Exists ".\\.now", "Root directory was not expected to exist")
            | (Error e, _) -> Assert.Fail(sprintf "Unexpected result: %A" e)
        finally
            if Directory.Exists ".\\.now" then
                Directory.Delete (".\\.now", true)
