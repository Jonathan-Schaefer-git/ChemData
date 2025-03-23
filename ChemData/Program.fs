open Npgsql.FSharp
open DensityParser
open Newtonsoft.Json
open DataSourcing
open ParserTemplate
open Pipeline
open System.IO
open FSharp.Data
open FSharp.Stats


type CidList = JsonProvider<"./Input/Compounds-CID-list.json">



let convertToJSON (data: (int * Parsing array) array) =
    let handleUnit (converter: 'T -> float) (valueOpt: 'T option) =
        match valueOpt with
        | Some value -> converter value |> box
        | None -> box null

    let getOptCelsius = 
        handleUnit (
            function
            | Celsius cel -> cel
            | _ -> failwith "Unexpected unit, earlier conversion failed"
        )

    let getOptPressure = 
            function
            | Some x -> x |> box
            | None -> box null

    let parseValue = function
        | Density d ->
            [| box d.Value; getOptCelsius d.Temperature |]
        | BoilingPoint bp ->
            [| bp.Temperature; getOptPressure bp.Pressure |]
        | MeltingPoint mp ->
            [| mp.Temperature; getOptPressure mp.Pressure |]




    let jsonObject =
        data
        |> Array.map (fun (cid, parsings) ->
            string cid, parsings |> Array.map parseValue
        )
        |> dict

    JsonConvert.SerializeObject(jsonObject)

let checkExistence (cid:int) =
    let file = Path.Combine($"{projectRoot}/JSON-FULL", $"{cid}.json")
    if File.Exists(file) then
        Some cid
    else
        None


let rec standardTemp (temp:Temperature) : Temperature =
    match temp with
    | Celsius x -> Celsius x
    | Kelvin x -> Celsius (x - 273.15)
    | Fahrenheit x -> Celsius ((x - 32.0) * 5.0 / 9.0)
    | Pair (x, y) -> Pair (standardTemp x, standardTemp y)


let standardizationFactor (unit:Units) =
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
    | MMHg -> 0.00133322





let standardize (s:Parsing) =
    match s with
    | Density d ->
        let standardTemp = 
            match d.Temperature with
            | Some t -> Some (standardTemp t)
            | None -> None


        let normalizedValue =
            match d.Units with
            | Some (massU, volumeU) ->
                let massFactor = standardizationFactor massU
                let volumeFactor = standardizationFactor volumeU
                d.Value * massFactor / volumeFactor
            | None -> d.Value

        Density { Value = normalizedValue; Units = Some (Gram, CubicCentimeter); Temperature = standardTemp}

    | BoilingPoint b ->
        
        let normalizedPressure =
            match b.Pressure with
            | Some p -> Some (p * standardizationFactor b.Unit)
            | None -> None


        BoilingPoint { Temperature = standardTemp b.Temperature; Pressure = b.Pressure }
    | _ -> failwith "Not implemented"





[<EntryPoint>]
let main _ =
    
    let densityCompounds = CidList.Load($"{projectRoot}/Input/Density-CID-list.json")
    
    
    let verifiedCompounds = densityCompounds |> Array.choose checkExistence




    verifiedCompounds
    |> Array.map(fun cid -> pipeline cid extractDensity)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.choose(fun (cid, data) -> 
        match data with
        | Some someData -> Some (cid, someData)
        | None -> None
    )
    |> Array.map(fun (cid, data) -> 
        (cid, data |> Array.map(fun x -> standardize x))
    )
    |> convertToJSON
    |> fun json -> File.WriteAllText($"{projectRoot}/Output/standardized.json", json)
    0