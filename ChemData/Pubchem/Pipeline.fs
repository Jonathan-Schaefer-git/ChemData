module Pipeline

open DataSourcing
open DensityParser
open BoilingPointParser
open MeltingPointParser
open RefractiveIndexParser
open ViscosityParser


type Parsing =
    | Density of DensityResult
    | BoilingPoint of BoilingPointResult
    | MeltingPoint of MeltingPointResult
    | RefractiveIndex of RefractiveIndexResult
    | Viscosity of ViscosityResult

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


let private extractionPipeline (parsingFunc: string -> Parsing option) (sec4: PubChemJSON.Section3 option) =
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

let extractViscosity (record: PubChemJSON.Root) =
    let viscosityWrapper str =
        match parseViscosity str with
        | Some v -> Some(Viscosity v)
        | None -> None

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Viscosity"
    |> extractionPipeline viscosityWrapper


let extractRefractiveIndex (record: PubChemJSON.Root) =
    let refractiveIndexWrapper str =
        match parseRefractiveIndex str with
        | Some x -> Some(RefractiveIndex x)
        | None -> None

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Refractive Index"
    |> extractionPipeline refractiveIndexWrapper


let extractDensity (record: PubChemJSON.Root) =
    let densityWrapper str =
        match parseDensity str with
        | Some x -> Some(Density x)
        | None -> None

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Density"
    |> extractionPipeline densityWrapper

let extractBoilingPoint (record: PubChemJSON.Root) =
    let boilingPointWrapper str =
        match parseBoilingPoint str with
        | Some x -> Some(BoilingPoint x)
        | None -> None

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Boiling Point"
    |> extractionPipeline boilingPointWrapper

let extractMeltingPoint (record: PubChemJSON.Root) =
    let meltingPointWrapper str =
        match parseMeltingPoint str with
        | Some x -> Some(MeltingPoint x)
        | None -> None

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Melting Point"
    |> extractionPipeline meltingPointWrapper


let pipeline (cid: int) (smiles:string) (parsingFunc: PubChemJSON.Root -> Parsing array option) =
    async {
        printfn "Getting %d" cid
        let! record = PubChemJSON.AsyncLoad $"{projectRoot}/JSON-FULL/{cid}.json"
        return cid, smiles, parsingFunc record
    }
