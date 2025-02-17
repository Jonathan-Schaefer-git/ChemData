module Database

open Npgsql
open Npgsql.FSharp

[<Literal>]
let connString = "Host=arch;Database=Chemistry;Username=chem;Password=1598"

let conn = Sql.connect connString
