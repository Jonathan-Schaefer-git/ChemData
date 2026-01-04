open DensityParser
open Newtonsoft.Json
open DataSourcing
open ParserTemplate
open BoilingPointParser
open MeltingPointParser
open RefractiveIndexParser
open ViscosityParser
open KovatsRetentionParser
open Pipeline
open System.IO
open FSharp.Data
open System.Threading
open System.Collections.Generic
open System
open System.Globalization

type CidList = JsonProvider<"./Input/Density-CID-list.json">



let convertToCsv (data: (int * string * Parsing array) array) =
    let handleUnit (converter: 'T -> float) (valueOpt: 'T option) =
        match valueOpt with
        | Some value -> converter value |> box
        | None -> box None

    let getOptCelsius =
        handleUnit (function
            | Celsius cel -> cel
            | Pair (Celsius temp1, Celsius temp2) -> (temp1, temp2) |> meanOfTuple
            | _ -> failwith "Unexpected unit, earlier conversion failed")

    let getOptPressure =
        function
        | Some x -> x |> box
        | None -> box None
 
    let parsingToCsvString (cid:int) (smiles:string) (p:Parsing) =
        match p with
        | Density d  ->
            [|$"{cid},{smiles},{d.Value},{box (getOptCelsius d.Temperature)}"|]
        | BoilingPoint bp ->
            [|$"{cid},{smiles},{box (getOptCelsius (Some bp.Temperature))},{box (getOptPressure bp.Pressure)}"|]
        | MeltingPoint mp ->
            [|$"{cid},{smiles},{box (getOptCelsius (Some mp.Temperature))},{box (getOptPressure mp.Pressure)}"|]
        | RefractiveIndex ri ->
            [|$"{cid},{smiles},{ri.Value},{box (getOptCelsius ri.Temperature)}"|]
        | Viscosity v ->
            [|$"{cid},{smiles},{v.Value},{box (getOptCelsius v.Temperature)}"|]
        | KovatsRetention kr ->
            kr.RI
            |> Array.map (fun rI -> $"{cid},{smiles},{rI}")
        

    let matchingHeaders =
        function
        | Density _  -> [| "CID,Smiles,Density,Temp" |]
        | BoilingPoint _ -> [| "CID,Smiles,BoilingPoint,Pressure" |]
        | MeltingPoint _ -> [| "CID,Smiles,MeltingPoint,Pressure" |]
        | RefractiveIndex _ -> [| "CID,Smiles,RefractiveIndex,Temp" |]
        | Viscosity _ -> [| "CID,Smiles,Viscosity,Temp" |]
        | KovatsRetention _ -> [| "CID,Smiles,KovatsRetention" |]


    let (_,_,headerParsings) = data[0]

    let rows =
        data
        |> Array.collect (fun (cid:int, smiles:string, parsings:Parsing array) ->
            parsings |> Array.collect (fun p -> parsingToCsvString cid smiles p))

    Array.append (matchingHeaders headerParsings[0]) rows

let checkExistence (cid: int) =
    let file = Path.Combine($"{projectRoot}/JSON-FULL", $"{cid}.json")
    if File.Exists(file) then Some cid else None


let rec standardTemp (temp: Temperature) : Temperature =
    match temp with
    | Celsius x -> Celsius x
    | Kelvin x -> Celsius(x - 273.15)
    | Fahrenheit x -> Celsius((x - 32.0) * 5.0 / 9.0)
    | Pair(x, y) -> Pair(standardTemp x, standardTemp y)


let standardTempOpt (temp: Temperature option) =
    match temp with
    | Some t -> Some (standardTemp t)
    | None -> None

let standardizationFactor (unit: Units) =
    match unit with
    // Volume
    | CubicCentimeter -> 1.0
    | CubicDecimeter -> 1000.0
    | CubicMeter -> 1000000.0
    | CubicMillimeter -> 0.001
    | Liter -> 1000.0
    | Deciliter -> 100.0
    | Centiliter -> 10.0
    | Milliliter -> 1.0
    // Weight
    | Gram -> 1.0
    | Milligram -> 0.001
    | Microgram -> 0.000001
    | Nanogram -> 0.000000001
    | Kilogram -> 1000.0
    | Ton -> 1000000.0
    // Area
    | SquareCentimeter -> 1.0
    | SquareMillimeter -> 0.01
    | SquareDecimeter -> 100.0
    | SquareMeter -> 10000.0
    | SquareKilometer -> 10000000000.0
    // Pressure
    | Bar -> 1.0
    | Atm -> 1.01325
    | HectoPascal -> 0.001
    | KiloPascal -> 0.01
    | Pascal -> 0.00001
    | MMHg -> 0.00133322
    // Length
    | Millimeter -> 0.1
    | Centimeter -> 1.0
    | Decimeter -> 10.0
    | Meter -> 100.0
    | Kilometer -> 100000.0
    // Time
    | Second -> 1.0
    | SecondSquared -> 1.0



let normalizePressure (pressure: float option) (unit: Units option) =
    match pressure, unit with
    | Some pressure, Some unit -> Some(pressure * standardizationFactor unit)
    | Some pressure, None -> Some(pressure * standardizationFactor MMHg)
    | None, Some unit -> failwith $"While parsing {unit} unit for pressure was given while no pressure could be parsed"
    | None, None -> None

let standardize (s: Parsing) =
    match s with
    | Density d ->

        let normalizedValue =
            match d.Units with
            | Some(massU, volumeU) ->
                let massFactor = standardizationFactor massU
                let volumeFactor = standardizationFactor volumeU
                d.Value * massFactor / volumeFactor
            | None -> d.Value

        Density
            { Value = normalizedValue
              Units = Some(Gram, CubicCentimeter)
              Temperature = standardTempOpt d.Temperature }

    | BoilingPoint b ->
        BoilingPoint
            { Temperature = standardTemp b.Temperature
              Pressure = normalizePressure b.Pressure b.Unit
              Unit = Some Bar }

    | MeltingPoint m ->
        MeltingPoint
            { Temperature = standardTemp m.Temperature
              Pressure = normalizePressure m.Pressure m.Unit
              Unit = Some Bar }

    | RefractiveIndex ri ->
        RefractiveIndex { Value = ri.Value; Temperature = standardTempOpt ri.Temperature}

    | Viscosity v ->
        Viscosity { Value = v.Value; Temperature = standardTempOpt v.Temperature}

    | KovatsRetention ri ->
        KovatsRetention ri

let filterBounds (lower:float) (upper:float) (p: Parsing) : bool =
    match p with
    | Density d -> d.Value >= lower && d.Value <= upper
    | BoilingPoint bp -> 
        standardTemp bp.Temperature |> function
        | Celsius t -> t >= lower && t <= upper
        | _ -> false
    | MeltingPoint mp -> 
        standardTemp mp.Temperature |> function
        | Celsius t -> t >= lower && t <= upper
        | _ -> false
    | _ -> true

let filterInfOrNaN (p: Parsing) : bool =
    match p with
    | Density d -> not (Double.IsInfinity d.Value || Double.IsNaN d.Value)
    | Viscosity v -> not (Double.IsInfinity v.Value || Double.IsNaN v.Value)
    | RefractiveIndex ri -> not (Double.IsInfinity ri.Value || Double.IsNaN ri.Value)
    | MeltingPoint mp -> 
        standardTemp mp.Temperature |> function
        | Celsius t -> not (Double.IsInfinity t || Double.IsNaN t)
        | _ -> false
    | BoilingPoint bp -> 
        standardTemp bp.Temperature |> function
        | Celsius t -> not (Double.IsInfinity t || Double.IsNaN t)
        | _ -> false
    | KovatsRetention kr ->
        kr.RI |> Array.forall (fun rI -> not (Double.IsInfinity rI || Double.IsNaN rI))
    | _ -> true


[<EntryPoint>]
let main _ =
    CultureInfo.DefaultThreadCurrentCulture <- CultureInfo.InvariantCulture
    let featurizer = [
        "Density", extractDensity, [|filterBounds 0.0 20; filterInfOrNaN|]
        "BoilingPoint", extractBoilingPoint, [| filterInfOrNaN |]
        "MeltingPoint", extractMeltingPoint, [| filterInfOrNaN|]
        "RefractiveIndex", extractRefractiveIndex, [| filterInfOrNaN |]
        "Viscosity", extractViscosity, [| filterInfOrNaN |]
        "KovatsRetention-StandardPolar", extractKovatsRetention StandardPolar, [||]
        "KovatsRetention-StandardNonPolar", extractKovatsRetention StandardNonPolar, [||]
        "KovatsRetention-SemiStandardNonPolar", extractKovatsRetention SemiStandardNonPolar, [||]
    ]

    let loadCompounds (comp:string) = 
        let compounds =
            CidList.Load $"{projectRoot}/Input/{comp}-CID-list.json"
            |> Array.map (fun x -> x.Item1, x.Item2.String)
            |> Array.map (fun (cid, smiles) -> 
                match checkExistence cid, smiles with
                | Some id, Some smiles -> Some (id, smiles)
                | _ -> None)

        let dataCount =
            compounds |> Array.sumBy (fun x-> if x.IsSome then 1 else 0)

        printfn $"Loading {dataCount} {comp} succeeded with a yield rate of { float dataCount / float compounds.Length}"

        Thread.Sleep(5000)

        compounds |> Array.choose (fun x -> if x.IsSome then x else None)
        

    let processCompounds (compounds: (int * string) array) (extractor: PubChemJSON.Root -> (Parsing -> bool) array -> Parsing array option) (name: string) (filters: (Parsing->bool) array) =
        let parsedData =
            compounds
            |> Array.map (fun (cid, smiles) -> pipeline cid smiles extractor)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.map (fun (cid,smiles,unfilteredData) -> cid, smiles, filters |> unfilteredData)
            |> Array.map (fun (cid, smiles, data) ->
                match data with
                | Some someData when someData.Length > 0 -> Some(cid, smiles, someData)
                | _ -> None)
        
        let dataYield = 
            float (parsedData |> Array.sumBy (fun x -> if x.IsSome then 1 else 0)) / float parsedData.Length

        printfn $"Parsing yielded {dataYield} of all valid molecules"        

        let preparedData =
            parsedData
            |> Array.choose (fun x -> if x.IsSome then x else None)
            |> Array.map (fun (cid, smiles, data) -> cid, smiles, data |> Array.map (fun x -> standardize x))
        
        preparedData
        |> convertToCsv
        |> fun csvLines -> File.WriteAllLines($"{projectRoot}/Output/{name}-standardized.csv", csvLines)

        printfn $"{preparedData.Length}"



    featurizer
    |> List.iter (fun (feature, extractor, filters) ->
        let compounds = loadCompounds feature
        processCompounds compounds extractor feature filters
    )

    // [
    //     "Index of refraction 1.4738 @ 25 °C/D"
    //     "Index of refraction: 1.4061 at 25 °C"
    // ] |> List.map (fun x -> x |> parseRefractiveIndex |> printfn "%A") |> ignore
    0
