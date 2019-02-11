module Now.Migrations.Now2_AddPlugin

open FSharpPlus
open Now.Sql

let run db =
    execNonQuery
        db
        ( Query
            ( """
              PRAGMA foreign_keys = ON;

              CREATE TABLE [Plugin]
                ( [Id] INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT
                , [Name] VARCHAR(64) NOT NULL UNIQUE
                );

              CREATE TABLE [PluginProperty]
                ( [Id] INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT
                , [Name] VARCHAR(64) NOT NULL UNIQUE
                , [PluginId] INTEGER
                , CONSTRAINT fkPluginPropertyPluginId
                    FOREIGN KEY (PluginId)
                    REFERENCES Plugin (Id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE
                );

              CREATE TABLE [TaskPlugin]
                ( [Id] INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT
                , [TaskId] INTEGER
                , [PluginId] INTEGER
                , CONSTRAINT fkTaskPluginTaskId
                    FOREIGN KEY (TaskId)
                    REFERENCES Task (Id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE
                , CONSTRAINT fkTaskPluginPluginId
                    FOREIGN KEY (PluginId)
                    REFERENCES Plugin (Id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE
                );

              CREATE TABLE [TaskPluginProperty]
                ( [Id] INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT
                , [TaskPluginId] INTEGER
                , [PluginPropertyId] INTEGER
                , [Value] VARCHAR(512) NOT NULL UNIQUE
                , CONSTRAINT fkTaskPluginPropertyTaskPluginId
                    FOREIGN KEY (TaskPluginId)
                    REFERENCES TaskPlugin (Id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE
                , CONSTRAINT fkTaskPluginPropertyPluginPropertyId
                    FOREIGN KEY (PluginPropertyId)
                    REFERENCES PluginProperty (Id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE
                );
              """
            , []
            )
        )
    |> map (Operators.konst ())