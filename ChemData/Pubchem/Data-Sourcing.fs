module DataSourcing

open System
open System.IO
open FSharp.Data
open JsonRepairUtils

type PubChemJSON = JsonProvider<"/home/jona/source/ChemData/ChemData/JSON-FULL/4.json">
type CompoundList = JsonProvider<"/home/jona/source/ChemData/ChemData/Input/Compound-labeled-all-sample.json">



let basePath = AppDomain.CurrentDomain.BaseDirectory
let projectRoot = Path.GetFullPath(Path.Combine(basePath, "../../.."))

let sdfPath = Path.Combine(projectRoot, "SDF")
let jsonPath = Path.Combine(projectRoot, "JSON-FULL")

printfn $"Project root: {projectRoot}"
printfn $"SDF Files at: {sdfPath} \n JSON files at: {jsonPath}"

let json = JsonRepair()


let repair (s: string) : string = json.Repair s


let private fetchCompoundList (filename: string) =
    File.ReadAllTextAsync(Path.Combine(projectRoot, filename)) |> Async.AwaitTask


let getCompoundsFromList (file: string) =
    async {
        let! list = fetchCompoundList file
        return list |> repair |> CompoundList.Parse
    }


let getCompoundData (cid: int) =
    async {
        let file = Path.Combine(jsonPath, $"{cid}.json")

        if File.Exists file then
            let! data = File.ReadAllTextAsync file |> Async.AwaitTask
            return Some(data |> PubChemJSON.Parse)
        else
            printfn $"File not found: {cid}"
            return None
    }
