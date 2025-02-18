module DataSourcing
open System
open System.Net
open System.IO
open FSharp.Data
open JsonRepairUtils

// Type providers for list intermediates and full structure records necessary for parsing
type CompoundList = JsonProvider<"./Input/Compound-labeled-all-sample.json">
type PubChemJSON = JsonProvider<"./JSON-FULL/4.json">


let basePath = AppDomain.CurrentDomain.BaseDirectory
let projectRoot = Path.GetFullPath(Path.Combine(basePath, "../../.."))

let sdfPath = Path.Combine(projectRoot, "SDF")
let jsonPath = Path.Combine(projectRoot, "JSON-FULL")

printfn $"Project root: {projectRoot}"
printfn $"SDF Files at: {sdfPath} \n JSON files at: {jsonPath}"

let json = JsonRepair()

let repair (s:string) =
    json.Repair(s)


let private fetchCompoundList (filename:string) =
    File.ReadAllTextAsync(Path.Combine(projectRoot, filename)) |> Async.AwaitTask
    

let getCompoundsFromList (file:string) =
    async {
        let! list = fetchCompoundList file
        return list |> repair |> CompoundList.Parse
    }

let getCompoundData (cid:int) =
    async {
        let file = Path.Combine(jsonPath, $"{cid}.json")
        if File.Exists(file) then
            let! data = File.ReadAllTextAsync(file) |> Async.AwaitTask
            return Some (PubChemJSON.Parse(data))
        else
            printfn $"File not found: {cid}"
            return None
    }


