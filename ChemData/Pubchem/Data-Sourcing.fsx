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
open System.Threading
open System
open System.Diagnostics


[<Literal>]
let sdfPath = "../SDF"
[<Literal>]
let jsonPath = "../JSON-FULL"

type Compound = JsonProvider<"../Input/Compound-labeled-all-sample.json">



//type CompoundList = JsonProvider<"../Input/PropertyList-sample.json">

let jsonRepair = JsonRepair()

let preparePropertyListToCid (prop:string) =
    let s = 
        jsonRepair.Repair(File.ReadAllText($"../Input/{prop}.json"))
        |> Compound.Parse
        |> Array.map (fun x -> x.Cid, x.Smiles)
    
    printfn $"Prepared {s.Length} entries for {prop}"
    s
    |> JsonConvert.SerializeObject 
    |> fun data -> File.WriteAllText($"../Input/{prop}-CID-list.json", data)

preparePropertyListToCid "Density"
preparePropertyListToCid "BoilingPoint"
preparePropertyListToCid "MeltingPoint"
preparePropertyListToCid "Solubility"
preparePropertyListToCid "RefractiveIndex"
preparePropertyListToCid "pH"
preparePropertyListToCid "KovatsRetention"
preparePropertyListToCid "LogP"
preparePropertyListToCid "HeatOfVaporization"
preparePropertyListToCid "HeatOfCombustion"
preparePropertyListToCid "FlashPoint"
preparePropertyListToCid "Dissociation"
preparePropertyListToCid "CollisionCrossSection"
preparePropertyListToCid "VaporPressure"
preparePropertyListToCid "VaporDensity"
preparePropertyListToCid "Viscosity"


let inpJson = File.ReadAllText("../Input/Compound-labeled-all-list.json")

//jsonRepair.ThrowExceptions <- true



try
    let repairedJson = jsonRepair.Repair(inpJson)
    File.WriteAllText("C:\\Users\\\Jonathan\\source\\repos\\ChemData\\ChemData\\Input\\Compound-labeled-all-list-repaired.json", repairedJson)
with
    | :? JsonRepairError as e -> printfn "Error: %A" e

//let reps =
//    File.ReadAllText("C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\Compound-labled-all-list-repaired.json")
//    |> JsonConvert.DeserializeObject<Compound>



let compounds = Compound.Load("../Input/Compound-labeled-all-list-repaired.json")
let compoundIds = compounds |> Array.map _.Cid


JsonConvert.SerializeObject(compoundIds) |> fun data -> File.WriteAllText("../Input/Compounds-CID-list.json", data)


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




let checkFileExistence (folderPath: string) (cid: int) =
    let filePath = Path.Combine(folderPath, $"{cid}.json")
    File.Exists(filePath)

    
type CompoundCid = JsonProvider<"../Input/KovatsRetention-CID-list.json">
type CompoundCidList = JsonProvider<"../Input/Compounds-CID-list.json">
let missingCids = CompoundCidList.Load("../Input/Compounds-CID-list.json") |> Array.filter (fun x -> not (checkFileExistence jsonPath x)) |> Array.distinct


//let missingKovatsCIDs = CompoundCid.GetSamples() |> Array.map _.Item1 |> Array.filter (fun x -> not (checkFileExistence jsonPath x)) |> Array.distinct
//printfn $"{missingKovatsCIDs.Length}"
printfn $"{missingCids.Length}"



type RateLimiter(rps: int) =
    let sem = new SemaphoreSlim(0, rps)
    let timer = new Timers.Timer(1000.0 / float rps)
    
    do
        timer.AutoReset <- true
        timer.Elapsed.Add(fun _ ->
            try 
                sem.Release() |> ignore
            with :? SemaphoreFullException -> ()
        )
        timer.Start()
    
    member _.WaitAsync() = sem.WaitAsync() |> Async.AwaitTask
    interface IDisposable with
        member _.Dispose() =
            timer.Dispose()
            sem.Dispose()



let fetchDataForCid (cids:int array) =
    let client = new Http.HttpClient()
    let stopwatch = Stopwatch.StartNew()


    let getbyCID cid =
        client.GetStringAsync($"https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/{cid}/JSON/") 
        |> Async.AwaitTask
    
    let writeData (cid:int) (data:string) =
        File.WriteAllTextAsync($"C:\\Users\\Jonathan\\source\\repos\\ChemData\\ChemData\\JSON-FULL\\{cid}.json", data) 
        |> Async.AwaitTask
    
    let pipeline cid =
        async {
            try
                printfn "Getting %d" cid
                let! record = getbyCID cid
                printfn "Writing %d" cid
                do! writeData cid record
                return true
            with ex ->
                printfn $"Failed CID {cid}: {ex.Message}"
                return false
        }
    
    /// Rate limiter using SemaphoreSlim and a timer that refills permits

    let runWithRateLimit rps (jobs: Async<'T>[]) =
        async {
            use limiter = new RateLimiter(rps)
            let! results =
                jobs
                |> Array.map (fun job ->
                    async {
                        do! limiter.WaitAsync()
                        return! job
                    })
                |> Async.Parallel
            return results
        }
    
    let statusFeedback =
        cids
        |> Array.map pipeline
        |> runWithRateLimit 6
        |> Async.RunSynchronously

    stopwatch.Stop()
    printfn "Finished in %ds" (stopwatch.ElapsedMilliseconds / 1000L)
    printfn $"Successes: {statusFeedback |> Array.filter (fun x -> x = true) |> Array.length}"
    printfn $"Failures: {statusFeedback |> Array.filter (fun x -> x = false) |> Array.length}"

    


missingCids
|> fetchDataForCid