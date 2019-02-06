module Now.Fs
open System.IO


(*
    Domain
*)


type FileType =
    | File
    | Directory


type File = FileType * string


(*
    Error
*)


type Error =
    | Exists of File
    | IoError of exn
    | NotFound of File


(*
    DSL
*)


type Instruction<'a> =
    | Append of string * string * 'a
    | Cp of File * string * 'a
    | Home of (string -> 'a)
    | Mk of File * 'a
    | Mv of File * string * 'a
    | Pwd of (string -> 'a)
    | Read of string * (string -> 'a)
    | Require of File * 'a
    | Rm of File * 'a
    | Write of string * string * 'a


type Fs<'a> =
    | Free of Instruction<Fs<'a>>
    | Pure of 'a


let private mapI f = function
    | Append(path, content, next) -> Append(path, content, next |> f)
    | Cp(file, dest, next) -> Cp(file, dest, next |> f)
    | Home next -> Home(next >> f)
    | Mk(file, next) -> Mk(file, next |> f)
    | Mv(file, dest, next) -> Mv(file, dest, next |> f)
    | Pwd next -> Pwd(next >> f)
    | Read(path, next) -> Read(path, next >> f)
    | Require (file, next) -> Require(file, next |> f)
    | Rm(file, next) -> Rm(file, next |> f)
    | Write(path, content, next) -> Write(path, content, next |> f)


let rec bind f = function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x


let map f = bind (f >> Pure)


type Fs<'a> with
    
    static member Return x = Pure x

    static member (>>=) (x, f) = bind f x


(*
    Interpreter
*)


open FSharpPlus.Operators
open System


let rec private interpret' = function
    | Pure a -> Ok a

    | Free(Append(path, content, next)) ->
        if IO.Directory.Exists path |> not then
            NotFound (File, path) |> Error
        else                
            IO.File.AppendAllText(path, content)
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Cp((Directory, path), dest, next)) ->
        
        let rec copyDir source dest =
            let files = IO.Directory.GetFiles source |> List.ofArray
            for file in files do
                let name = IO.Path.GetFileName(file)
                IO.File.Copy(file, IO.Path.Combine(dest, name))
            let directories = IO.Directory.GetDirectories(source)
            for directory in directories do
                let name = IO.Path.GetFileName(directory)
                copyDir directory (IO.Path.Combine(dest, name))
        
        if IO.Directory.Exists dest then
            Exists (Directory, dest) |> Error
        elif IO.Directory.Exists path |> not then
            NotFound (Directory, path) |> Error
        else
            copyDir path dest
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Cp((File, path), dest, next)) ->
        if IO.File.Exists path then
            Exists (File, dest) |> Error
        elif IO.File.Exists path |> not then
            NotFound (File, path) |> Error
        else
            IO.File.Copy(path, dest)
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Home next) ->
        Environment.SpecialFolder.UserProfile
        |> Environment.GetFolderPath
        |> next
        |> interpret'

    | Free(Mk((Directory, path), next)) ->
        if IO.Directory.Exists path then
            Exists (File, path) |> Error
        else
            IO.Directory.CreateDirectory path |> ignore
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Mk((File, path), next)) ->
        if IO.File.Exists path then
            Exists (File, path) |> Error
        else
            IO.File.Create path |> ignore
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Mv((Directory, path), dest, next)) ->
        if IO.Directory.Exists dest then
            Exists (Directory, dest) |> Error
        elif IO.Directory.Exists path |> not then
            NotFound (Directory, path) |> Error
        else
            IO.Directory.Move(path, dest)
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Mv((File, path), dest, next)) ->
        if IO.File.Exists dest then
            Exists (File, dest) |> Error
        elif IO.File.Exists path |> not then
            NotFound (File, path) |> Error
        else
            IO.File.Move(path, dest)
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Pwd next) ->
        Directory.GetCurrentDirectory()
        |> next
        |> interpret'

    | Free(Read(path, next)) ->
        if IO.Directory.Exists path |> not then
            NotFound (File, path) |> Error
        else                
            IO.File.ReadAllText path |> Ok
        |> Result.map next
        >>= interpret'

    | Free(Require((Directory, path), next)) ->
        if IO.Directory.Exists path |> not then
            NotFound (Directory, path) |> Error
        else
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Require((File, path), next)) ->
        if IO.File.Exists path |> not then
            NotFound (File, path) |> Error
        else
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Rm((Directory, path), next)) ->
        if IO.Directory.Exists path |> not then
            NotFound (Directory, path) |> Error
        else
            IO.Directory.Delete(path, true)
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Rm((File, path), next)) ->
        if IO.File.Exists path |> not then
            NotFound (File, path) |> Error
        else
            IO.File.Delete path
            Ok ()
        |> Result.map (konst next)
        >>= interpret'

    | Free(Write(path, content, next)) ->
        if IO.Directory.Exists path |> not then
            NotFound (File, path) |> Error
        else                
            IO.File.WriteAllText(path, content)
            Ok ()
        |> Result.map (konst next)
        >>= interpret'


let interpret program =
    try
        interpret' program
    with
    | :? IO.IOException as e -> IoError e |> Error


(*
    Public API
*)


let append path content = Append(path, content, Pure ()) |> Free
let cp file dest = Cp(file, dest, Pure ()) |> Free
let cpdir name dest = Cp((Directory, name), dest, Pure ()) |> Free
let cpfile name dest = Cp((File, name), dest, Pure ()) |> Free
let home = Home Pure |> Free
let mk file = Mk(file, Pure ()) |> Free
let mkdir name = Mk((Directory, name), Pure ()) |> Free
let mkfile name = Mk((File, name), Pure ()) |> Free
let mv file dest = Mv(file, dest, Pure ()) |> Free
let mvdir name dest = Mv((Directory, name), dest, Pure ()) |> Free
let mvfile name dest = Mv((File, name), dest, Pure ()) |> Free
let pwd = Pwd Pure |> Free
let read path = Read(path, Pure) |> Free
let require file = Require(file, Pure ()) |> Free
let requireDir name = Require((Directory, name), Pure ()) |> Free
let requireRile name = Require((File, name), Pure ()) |> Free
let rm file = Rm(file, Pure ()) |> Free
let rmdir name = Rm((Directory, name), Pure ()) |> Free
let rmfile name = Rm((File, name), Pure ()) |> Free
let write path content = Write(path, content, Pure ())