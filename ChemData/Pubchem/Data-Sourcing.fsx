#r "nuget:FSharp.Data"
#r "nuget:FSharp.Stats"
#r "nuget:Newtonsoft.Json"
#r "nuget:JsonRepairUtils"

open System.IO
open FSharp.Data
open FSharp.Stats
open Newtonsoft.Json
open JsonRepairUtils
open System.Net
open System.Diagnostics

type Compound = JsonProvider<"../Compound-labled-all-sample.json">





//let inpJson = File.ReadAllText("../Compound-labled-all-list.json")


//let jsonRepair = JsonRepair()

//jsonRepair.ThrowExceptions <- true



//try
//    let repairedJson = jsonRepair.Repair(inpJson)
//    File.WriteAllText("C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\Compound-labled-all-list-repaired.json", repairedJson)
//with
//    | :? JsonRepairError as e -> printfn "Error: %A" e

//let reps =
//    File.ReadAllText("C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\Compound-labled-all-list-repaired.json")
//    |> JsonConvert.DeserializeObject<Compound>



let compounds = Compound.Load("../Compound-labled-all-list-repaired.json")
let compoundIds = compounds |> Array.map _.Cid


JsonConvert.SerializeObject(compoundIds) |> fun data -> File.WriteAllText("../Compounds-CID-list.json", data)


[<Literal>]
let sdfPath = "../SDF"
[<Literal>]
let jsonPath = "../JSON-FULL"


let deleteAllFiles (folderPath: string) =
    Directory.EnumerateFiles(folderPath)
    |> Seq.iter (fun path ->
        try
            File.Delete(path)
            printfn $"Deleted: {path}"
        with ex ->
            printfn $"Error deleting {path}: {ex.Message}"
    )

deleteAllFiles sdfPath
deleteAllFiles jsonPath



let client = new Http.HttpClient()
let stopwatch = Stopwatch.StartNew()

printfn "Start"


let getbyCID cid =
    client.GetStringAsync($"https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/{cid}/JSON/") |> Async.AwaitTask

let getSDFbyCID cid =
    client.GetStringAsync($"https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/CID/{cid}/record/SDF?record_type=3d") |> Async.AwaitTask

let writeData (cid:int) (data:string) =
    File.WriteAllTextAsync($"C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\JSON-FULL\\{cid}.json", data) |> Async.AwaitTask

let writeStructure (cid:int) (data:string) =
    File.WriteAllTextAsync($"C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\SDF\\{cid}.sdf", data) |> Async.AwaitTask


let pipeline cid =
    async {
        printfn "Getting %d" cid


        try
            let! record = getbyCID cid
            let! sdf = getSDFbyCID cid
            do! writeData cid record
            do! writeStructure cid sdf
            do! Async.Sleep 500
            return true
        with
            | ex -> 
                printfn $"Failed obtaining all data associated with CID: {cid}"
                printfn $"Error: {ex.Message}"
                return false
    }

let statusFeedback = 
    compoundIds
    |> Array.map pipeline
    |> Async.Sequential
    |> Async.RunSynchronously
    
printfn "Finished in %ds" (stopwatch.ElapsedMilliseconds / 1000L)
printfn $"Successes: {statusFeedback |> Array.filter (fun x -> x = true) |> Array.length}"
printfn $"Failures: {statusFeedback |> Array.filter (fun x -> x = false) |> Array.length}"
