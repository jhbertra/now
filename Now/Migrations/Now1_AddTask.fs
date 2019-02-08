module Now.Migrations.Now1_AddTask

open FSharpPlus
open Now.Sql

let run db =
    execNonQuery
        db
        ( Query
            ( """
              CREATE TABLE [Task]
                ( [Id] INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT
                , [Name] VARCHAR(64) NOT NULL UNIQUE
                )
              """
            , []
            )
        )
    |> map (Operators.konst ())