module Now.Migrations.Local1_AddTask
open System.Data.SQLite

let run (connection : SQLiteConnection) =
    use command = connection.CreateCommand()
    command.CommandText <-
        """
        CREATE TABLE [Task]
          ( [Id] INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT
          , [Name] VARCHAR(64) NOT NULL UNIQUE
          )
        """
    command.ExecuteNonQuery() |> ignore