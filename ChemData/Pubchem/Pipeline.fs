module Pipeline
open System.IO
open FParsec
open FSharp.Data
open DataSourcing

// 0.995
// 0.9950 g/cu cm at 25 °C
// 0.9950 g/cm^3 at 25 °C
// 1.000 at 277K


let getSection (header:string) (record:PubChemJSON.Record) =
    record.Section
    |> Array.filter(fun secs -> secs.TocHeading = header)
    |> Array.tryHead

let getSubSection (header:string) (sec:(PubChemJSON.Section) option) =
    match sec with
    | Some subsec ->
        subsec.Section
        |> Array.filter(fun secs -> secs.TocHeading = header)
        |> Array.tryHead
    | None -> 
        printfn $"Failed to get subsection: {header}"
        None


let getPropertySection (header:string) (subsec:PubChemJSON.Section2 option) = 
    match subsec with
    | Some propsec ->
        propsec.Section
        |> Array.filter(fun secs -> secs.TocHeading = header)
        |> Array.tryHead
    | None -> 
        printfn $"Failed to get subsection: {header}"
        None

let getFile (cid:int) =
    async{
        if File.Exists($"C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\JSON-FULL\\{cid}.json") then
            let! fileContent = File.ReadAllTextAsync($"C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\JSON-FULL\\{cid}.json") |> Async.AwaitTask
            return Some (PubChemJSON.Parse(fileContent))
        else
            printfn $"File not found: {cid}"
            return None
    }



let extractionPipeline (record:PubChemJSON.Root) = 
    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Density"
    |> function
        | Some x -> 
            if Array.length x.Information = 0 then
                None
            else
                x.Information
                |> Array.choose(fun info ->
                    info
                    |> _.Value.StringWithMarkup
                    |> Array.tryHead
                    |> function
                        | Some x -> 
                            x.String.String
                        | None -> None
                )
                |> Array.choose parseDensity
                |> Some
        | None -> None






type DensityList = JsonProvider<"C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\density-list-repaired.json">

let densityCids =
    DensityList.Load($"{projectRoot}/density-list-repaired.json")
    |> Array.map _.Cid

let processDensity (cid:int) =
    async {
        let! record = getFile cid
        match record with
        | Some record ->
            match extractionPipeline record with
            | Some density -> 
                return Some (cid, density)
            | None ->
                return None
        | None -> 
            return None
    }