module Pipeline
open System
open DataSourcing
open DensityParser
open BoilingPointParser
open MeltingPointParser
open RefractiveIndexParser
open ViscosityParser
open KovatsRetentionParser

type Parsing =
    | Density of DensityResult
    | BoilingPoint of BoilingPointResult
    | MeltingPoint of MeltingPointResult
    | RefractiveIndex of RefractiveIndexResult
    | Viscosity of ViscosityResult
    | KovatsRetention of KovatsRetentionResult

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


let applyFilters (filterFuncs: (Parsing -> bool) array) (data: Parsing) =
    if filterFuncs |> Array.forall (fun f -> f data) then Some data else None


let extractViscosity (record: PubChemJSON.Root) (filters:(Parsing -> bool) array) =
    let viscosityWrapper str =
        parseViscosity str |> Option.bind (fun v -> applyFilters filters (Viscosity v))

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Viscosity"
    |> extractionPipeline viscosityWrapper


let extractKovatsRetention (columnTypeToExtract:ColumnType) (record: PubChemJSON.Root) (filters:(Parsing -> bool) array) =
    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Kovats Retention Index"
    |> function
        | Some sec3 -> 
            if sec3.Information.Length = 0 then
                None
            else
                sec3.Information 
                |> Array.map (fun x -> x.Name.Value, (x.Value.Number |> Array.map float))
                |> Array.choose (fun (columnType, values) -> 
                    match identifyColumnType columnType with
                    | Some cT ->
                        if cT = columnTypeToExtract then
                            Some (KovatsRetention { ColumnType = cT; RI = values})
                        else
                            None
                    | None -> None
                        // failwith $"Couldnt match column type string {columnType} with data {values}"
                    )
                |> Some
        | None -> failwith "Nothing here"


let extractRefractiveIndex (record: PubChemJSON.Root) (filters: (Parsing -> bool) array) =
    let refractiveIndexWrapper str =
        parseRefractiveIndex str |> Option.bind (fun refI -> applyFilters filters (RefractiveIndex refI))

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Refractive Index"
    |> extractionPipeline refractiveIndexWrapper


let extractDensity (record: PubChemJSON.Root) (filters: (Parsing -> bool) array) =
    let densityWrapper str =
        parseDensity str |> Option.bind (fun d -> applyFilters filters (Density d))

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Density"
    |> extractionPipeline densityWrapper

let extractBoilingPoint (record: PubChemJSON.Root) (filters: (Parsing -> bool) array) =
    let boilingPointWrapper str =
        parseBoilingPoint str |> Option.bind (fun b -> applyFilters filters (BoilingPoint b))

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Boiling Point"
    |> extractionPipeline boilingPointWrapper

let extractMeltingPoint (record: PubChemJSON.Root) (filters: (Parsing -> bool) array) =
    let meltingPointWrapper str =
        parseMeltingPoint str |> Option.bind (fun m -> applyFilters filters (MeltingPoint m))

    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Melting Point"
    |> extractionPipeline meltingPointWrapper


let pipeline (cid: int) (smiles: string) (parsingFunc: PubChemJSON.Root -> (Parsing -> bool) array -> Parsing array option) (filters: (Parsing -> bool) array) =
    async {
        printfn "Getting %d" cid
        try
            let! record = PubChemJSON.AsyncLoad $"{projectRoot}/JSON-FULL/{cid}.json"
            return cid, smiles, filters |> (parsingFunc record)
        with ex ->
            let msg = sprintf "Failed obtaining all data associated with CID %d: %s" cid ex.Message
            return raise (System.Exception(msg, ex))
    }

