open Npgsql.FSharp
open Pubchem
open Newtonsoft.Json



type Source =
    | Pubchem = 1
    | ChemSpider = 2




module Database =

    let private connectionString = 
        Sql.host "arch"
        |> Sql.database "Chemistry"
        |> Sql.username "chem"
        |> Sql.password "1598"
        |> Sql.port 5432
        |> Sql.formatConnectionString



    let insertMolecule (inchi: string) =
        connectionString
        |> Sql.connect
        |> Sql.query
            """
            INSERT INTO molecules (inchi)
            VALUES (@inchi)
            RETURNING id
            """
        |> Sql.parameters [ "@inchi", Sql.string inchi ]
        |> Sql.executeRow (fun read -> read.int "id")



    let insertDensity (moleculeId: int) (density: float) =
        connectionString
        |> Sql.connect
        |> Sql.query
            """
            INSERT INTO density (id, density)
            VALUES (@id, @density)
            ON CONFLICT (id) DO UPDATE SET density = @density
            """
        |> Sql.parameters [
            "@id", Sql.int moleculeId
            "@density", Sql.double density
        ]
        |> Sql.executeNonQuery



    //let insertMoleculeWithProperties (inchi: string) (density: float option) =
    //    match insertMolecule inchi with
    //    | Ok [ moleculeId ] ->
    //        density |> Option.iter (fun d ->
    //            insertDensity moleculeId d |> ignore
    //        )
    //        Ok moleculeId
    //    | Error error -> Error error
    //    | _ -> Error "Unexpected result when inserting molecule"

// Usage example
//let result = 
//    Database.insertMoleculeWithProperties
//        "InChI=1S/H2O/h1H2"
//        (Some 1.0)  // Optional density value

//match result with
//| Ok moleculeId -> printfn $"Inserted molecule with ID: {moleculeId}"
//| Error e -> printfn $"Error: {e}"

[<EntryPoint>]
let main _ =
    let densityData =
        densityCids
        |> Array.map (fun cid -> processDensity cid)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.choose (fun x -> x)


    printfn $"Parsed {densityData.Length} successfully"
    0