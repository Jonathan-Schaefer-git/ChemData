module Pubchem

open NCDK.Smiles
open NCDK.Default

open System.IO
open System.Diagnostics
open System.Net

open FParsec
open FSharp.Data

type Temperature = 
    | Celsius of float
    | Kelvin of float
    | Fahrenheit of float
    | Unknown of string

    
type Units =
    | Gram
    | Kilogram
    | CubicCentimeter
    | CubicMeter
    | Liter
    | Milliliter
    | CubicDecimeter


type DensityResult = {
    Value: float
    Units: (Units * Units) option
    Temperature: Temperature option
}


type PubChemJSON = JsonProvider<"https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/702/JSON/">

let getSection (header:string) (record:PubChemJSON.Record) =
    record.Section
    |> Array.filter(fun secs -> secs.TocHeading = header)
    |> Array.tryHead
    |> function
        | Some sec ->
            sec
        | None -> failwith $"Couldn't find the section with the header '{header}'"

let getSubSection (header:string) (sec:PubChemJSON.Section) =
    sec.Section
    |> Array.filter(fun secs -> secs.TocHeading = header)
    |> Array.tryHead
    |> function
        | Some sec ->
            sec
        | None -> failwith $"Couldn't find the subsection with the header '{header}'"


let getPropertySection (header:string) (subsec:PubChemJSON.Section2) = 
    subsec.Section
    |> Array.filter(fun secs -> secs.TocHeading = header)
    |> Array.tryHead
    |> function
        | Some sec ->
            sec
        | None -> failwith $"Couldn't find the subsection with the header '{header}'"


// 0.995
// 0.9950 g/cu cm at 25 °C
// 0.9950 g/cm^3 at 25 °C
// 1.000 at 277K
let parseInput (str:string) : DensityResult option =
    let floatOrInt : Parser<float,unit> =
        pfloat <|> (pint32 |>> float)

    let tempC =
        floatOrInt .>> spaces .>> pchar '°' .>> spaces .>> pstring "C" |>> Celsius

    let tempK =
        floatOrInt .>> spaces .>> pchar 'K' |>> Kelvin

    let tempF =
        floatOrInt .>> spaces .>> pchar '°' .>> spaces .>> pchar 'F' |>> Fahrenheit

    let temp = choice [
        attempt tempC
        attempt tempK
        attempt tempF
        floatOrInt .>> restOfLine false |>> (fun t -> Unknown(string t))
    ]

    let densityUnit =
        choice [
            pstring "g" >>% Gram
            pstring "kg" >>% Kilogram
        ] 
        .>> spaces .>> pchar '/' .>> spaces .>>. 
        choice [
            
            pstring "m³" >>% CubicMeter
            pstring "dm³" >>% CubicDecimeter
            
            pstring "cm³" >>% CubicCentimeter
            pstring "cm3" >>% CubicCentimeter
            pstring "cu" >>. spaces >>. pstring "cm" >>% CubicCentimeter
            pstring "cc" >>% CubicCentimeter
            
            pstring "l" >>% Liter
            pstring "L" >>% Liter
            
            pstring "ml" >>% Milliliter
            pstring "mL" >>% Milliliter
            
            
        ]

    let meanOfTuple ((x,y): float * float) = (x + y) / 2.0

    let eol = spaces .>> eof

    let pRangeOrFloat = choice [
        attempt (floatOrInt .>> notFollowedBy (pchar '-'))
        attempt (floatOrInt .>> spaces .>> skipChar '-' .>> spaces .>>. floatOrInt |>> meanOfTuple)
        attempt (floatOrInt .>> spaces .>> skipString "to" .>> spaces .>>. floatOrInt |>> meanOfTuple)
    ]

    let tempQuantifier = 
        choice [
            pstring "at" .>> spaces
            pstring "Temp" .>> spaces .>> pstring "of" .>> spaces .>> pstring "max" .>> spaces .>> pstring "density"
        ]

    let pDensity =
        pipe3
            pRangeOrFloat
            (spaces >>. opt densityUnit)
            (spaces >>. opt (tempQuantifier >>. spaces >>. temp))
            (fun value unit temp -> 
                { Value = value
                  Units = unit
                  Temperature = temp })

    let s = 
        spaces >>. choice [
            attempt (pDensity .>> eol)
            attempt (skipManyTill anyChar pDensity >>. pDensity .>> eol)
            attempt (pRangeOrFloat .>> eol |>> fun v -> 
                { Value = v; Units = None; Temperature = None })
        ]
    
    match run s str with
    | Success (data, _, _) -> 
        printfn "Input: %s \nParsed: %A" str data
        if data.Value <> 0.0 then Some data else None
    | Failure (msg, _, _) ->
        printfn "Parse failed for '%s': %s" str msg
        None


let getDensities (doc:PubChemJSON.Record) =
    getSection "Chemical and Physical Properties" doc
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Density"
    |> _.Information
    |> Array.map(fun x -> printfn "%A" x)

let basePath = System.AppDomain.CurrentDomain.BaseDirectory
let projectRoot = Path.GetFullPath(Path.Combine(basePath, "../../.."))
printfn "Project root: %s" projectRoot


let sdfPath = Path.Combine(projectRoot, "SDF")

let jsonPath = Path.Combine(projectRoot, "JSON-FULL")


type Compound = JsonProvider<"Compound-labled-all-sample.json">

type CompoundIds = JsonProvider<"Compounds-CID-list.json">



// Data acquisition
let generateData () =
    
    let compoundIds = CompoundIds.Load(Path.Combine(projectRoot, "Compounds-CID-list.json"))
    let client = new Http.HttpClient()
    let stopwatch = Stopwatch.StartNew()
    
    let startCid = 131814851


    let (_, ahh) = compoundIds |> Array.partition(fun x -> x < startCid)

    printfn $"Start at {startCid}"


    printfn "Start"
    let getbyCID cid =
        client.GetStringAsync($"https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/{cid}/JSON/") |> Async.AwaitTask
    
    let getSDFbyCID cid =
        client.GetStringAsync($"https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/CID/{cid}/record/SDF?record_type=3d") |> Async.AwaitTask
    
    let writeData (cid:int) (data:string) =
        File.WriteAllTextAsync($"{jsonPath}/{cid}.json", data) |> Async.AwaitTask
    
    let writeStructure (cid:int) (data:string) =
        File.WriteAllTextAsync($"{sdfPath}/{cid}.sdf", data) |> Async.AwaitTask
    
    
    let pipeline cid =
        async {
            printfn "Getting %d" cid    
            try
                let! record = getbyCID cid
                let! sdf = getSDFbyCID cid
                do! writeData cid record
                do! writeStructure cid sdf
                do! Async.Sleep 500
                return true
            with
                | ex -> 
                    printfn $"Failed obtaining all data associated with CID: {cid}"
                    printfn $"Error: {ex.Message}"
                    return false
        }
    
    let statusFeedback = 
        ahh
        |> Array.map pipeline
        |> Async.Sequential
        |> Async.RunSynchronously
        
    printfn "Finished in %ds" (stopwatch.ElapsedMilliseconds / 1000L)
    printfn $"Successes: {statusFeedback |> Array.filter (fun x -> x = true) |> Array.length}"
    printfn $"Failures: {statusFeedback |> Array.filter (fun x -> x = false) |> Array.length}"