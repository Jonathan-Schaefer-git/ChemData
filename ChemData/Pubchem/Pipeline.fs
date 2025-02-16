module Pipeline
open System.IO
open FParsec
open FSharp.Data
open DataSourcing
open DensityParser


type Parsing =
    | Density of DensityResult


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


let extractionPipeline (parsingFunc:string -> Parsing option) (sec3:PubChemJSON.Section3 option) = 
    match sec3 with
    | Some x -> 
        Array.isEmpty x.Information
        |> function
            | true -> None
            | false ->
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
                |> Array.choose parsingFunc
                |> Some
    | None -> None


let extractDensity (record:PubChemJSON.Root) =
    let densityWrapper str =
        match parseDensity str with
        | Some x -> Some (Density x)
        | None -> None

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Density"
    |> extractionPipeline densityWrapper


