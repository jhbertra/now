namespace Now.Test

open NUnit.Framework

open Now.Now

type EnvironmentTests () =

    [<Test>]
    member this.Install_NotInstalled_Installs () =
        TestUtilities.runTest
            [| "install" |]
            []
            (Ok
                 { TestResult.empty with
                    databaseExists = true
                    rootDirExists = true }
            )

    [<Test>]
    member this.Install_Installed_Fails () =
        TestUtilities.runSetup [| "install" |] []
        TestUtilities.runTest
            [| "install" |]
            []
            (Error (EnvironmentError Now.Env.RootDirExists))

    [<Test>]
    member this.Uninstall_Installed_Fails () =
        TestUtilities.runSetup [| "install" |] []
        TestUtilities.runTest
            [| "uninstall" |]
            []
            (Ok TestResult.empty)

    [<Test>]
    member this.Uninstall_NotIstalled_Fails () =
        TestUtilities.runTest
            [| "uninstall" |]
            []
            (Error (EnvironmentError Now.Env.RootDirMissing))