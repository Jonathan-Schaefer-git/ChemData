#r "nuget:FSharp.Data"
#r "nuget:FParsec"
#r "nuget: JsonRepairUtils"


open System
open FParsec
open FSharp.Data
open System.IO
open JsonRepairUtils

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

let sample = PubChemJSON.GetSample()

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


getSection "Chemical and Physical Properties" sample.Record
|> getSubSection "Experimental Properties"
|> getPropertySection "Density"
|> _.Information



// 0.995
// 0.9950 g/cu cm at 25 °C
// 0.9950 g/cm^3 at 25 °C
// 1.000 at 277K
let parseDensity (str:string) : DensityResult option =
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
        spaces .>> choice [
            pstring "@" .>> spaces 
            pstring "at" .>> spaces
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
            attempt (pRangeOrFloat .>> eol |>> fun v -> { Value = v; Units = None; Temperature = None })
        ]
    
    match run s str with
    | Success (data, _, _) -> 
        printfn "Input: %s \nParsed: %A" str data
        if data.Value <> 0.0 then Some data else None
    | Failure (msg, _, _) ->
        printfn "Parse failed for '%s': %s" str msg
        None




let getFile (cid:int) =
    async{
        let! fileContent = File.ReadAllTextAsync($"C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\JSON-FULL\\{cid}.json") |> Async.AwaitTask
        return PubChemJSON.Parse(fileContent)
    }

let extractionPipeline (record:PubChemJSON.Root) = 
    getSection "Chemical and Physical Properties" record.Record
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Density"
    |> _.Information
    |> Array.choose(fun info ->
        info
        |> _.Value.StringWithMarkup[0].String
        |> _.String
    )
    |> Array.map parseDensity


type DensityList = JsonProvider<"../density-list-repaired.json">

let densityCids =
    DensityList.Load("../density-list-repaired.json")
    |> Array.map _.Cid


let process (cids:int array) =
    async {
        cids
        |> Array.map (fun cid -> getFile cid))
    } 
    |> Async.Parallel 
    |> Async.RunSynchronously