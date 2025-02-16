module Database

open FSharp.Data.Sql

[<Literal>]
let dbVen = Common.DatabaseProviderTypes.POSTGRESQL

[<Literal>]
let connString = "Host=arch;Database=Chemistry;Username=chem;Password=1598"

let [<Literal>] resPath = @"C:\Users\jonat\.nuget\packages\npgsql\9.0.2\lib\"

let [<Literal>] indivAmount = 1000

let [<Literal>] useOptTypes  = Common.NullableColumnType.OPTION

let [<Literal>] owner = "public, admin, references"

type postgres = SqlDataProvider<dbVen, connString, "">