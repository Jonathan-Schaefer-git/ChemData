#r "nuget:FSharp.Data"
#r "nuget:FParsec"
#r "nuget: JsonRepairUtils"

open FParsec
open FSharp.Data

type PubChemJSON = JsonProvider<"../JSON-FULL/4.json">

let getSection (header: string) (record: PubChemJSON.Record) =
    record.Section
    |> Array.filter (fun secs -> secs.TocHeading = header)
    |> Array.tryHead

let getSubSection (header: string) (sec: (PubChemJSON.Section) option) =
    match sec with
    | Some subsec ->
        subsec.Section
        |> Array.filter (fun secs -> secs.TocHeading = header)
        |> Array.tryHead
    | None ->
        printfn $"Failed to get subsection: {header}"
        None

let getPropertySection (header: string) (subsec: PubChemJSON.Section2 option) =
    match subsec with
    | Some propsec ->
        propsec.Section
        |> Array.filter (fun secs -> secs.TocHeading = header)
        |> Array.tryHead
    | None ->
        printfn $"Failed to get property section: {header}"
        None



let sample = PubChemJSON.Load("../JSON-FULL/4.json")



let sect =
    sample.Record
    |> getSection "Chemical and Physical Properties"
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Kovats Retention Index"
    |> function
        | Some sec3 -> sec3.Information |> Array.map (fun x -> x.Name.Value, x.Value.Number)
        | None -> failwith "Nothing here"


let private extractionPipeline (parsingFunc: string -> float option) (sec4: PubChemJSON.Section3 option) =
    match sec4 with
    | Some x ->
        Array.isEmpty x.Information
        |> function
            | true -> None
            | false ->
                x.Information
                |> Array.choose (fun info ->
                    info
                    |> _.Value.StringWithMarkup
                    |> Array.tryHead
                    |> function
                        | Some x -> x.String.String
                        | None -> None)
                |> Array.choose parsingFunc
                |> Some
    | None -> None

