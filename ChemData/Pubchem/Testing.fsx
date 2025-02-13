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
            pstringCI "g" >>% Gram
            pstringCI "kg" >>% Kilogram
        ] 
        .>> spaces .>> pchar '/' .>> spaces .>>. 
        choice [
            
            pstringCI "m³" >>% CubicMeter
            pstringCI "dm³" >>% CubicDecimeter
            
            pstringCI "cm³" >>% CubicCentimeter
            pstringCI "cm3" >>% CubicCentimeter
            pstringCI "cu" >>. spaces >>. pstringCI "cm" >>% CubicCentimeter
            pstringCI "cc" >>% CubicCentimeter
            
            pstringCI "l" >>% Liter
            
            pstringCI "ml" >>% Milliliter            
            
        ]

    let meanOfTuple ((x,y): float * float) = (x + y) / 2.0

    let eol = spaces .>> eof
    
    let pRangeIndicators = 
        spaces .>> choice [
            pstringCI "to"
            pstring "-"
        ] .>> spaces
        
    let pRangeOrFloat = choice [
        attempt (floatOrInt .>> spaces .>> notFollowedBy (pchar '-'))
        attempt (floatOrInt .>> pRangeIndicators .>>. floatOrInt |>> meanOfTuple)
        attempt (floatOrInt .>> spaces .>> skipStringCI "to" .>> spaces .>>. floatOrInt |>> meanOfTuple)
    ]

    

    let tempQuantifier = 
        spaces .>> choice [
            
            pstring "@" .>> spaces 
            pstringCI "at" .>> spaces
        ]

    let tempPair = 
        temp .>> opt (spaces >>. pchar '/' >>. spaces >>. temp)
    
    let pDensity =
        pipe3
            pRangeOrFloat
            (spaces >>. opt densityUnit)
            (spaces >>. opt (tempQuantifier >>. tempPair))
            (fun value unit temp -> 
                { Value = value
                  Units = unit
                  Temperature = temp })


    let s = 
        spaces >>. choice [
            attempt (pDensity .>> eol)
            attempt (skipManyTill anyChar pDensity >>. pDensity .>> eol)
            attempt (pRangeOrFloat .>> eol |>> fun v -> { Value = v; Units = None; Temperature = None })
            attempt (pDensity .>> pchar '(' .>> skipManyTill anyChar (pchar ')') .>> eol)
        ]

    match run s str with
    | Success (data, _, _) ->
        if data.Value <> 0.0 then Some data else None
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with {msg}"
        None




let getFile (cid:int) =
    async{
        if File.Exists($"C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\JSON-FULL\\{cid}.json") then
            let! fileContent = File.ReadAllTextAsync($"C:\\Users\\jonat\\source\\repos\\ChemData\\ChemData\\JSON-FULL\\{cid}.json") |> Async.AwaitTask
            return Some (PubChemJSON.Parse(fileContent))
        else
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
                    |> _.Value.StringWithMarkup[0].String
                    |> _.String
                )
                |> Array.map parseDensity
                |> Some
        | None -> None



type DensityList = JsonProvider<"../density-list-repaired.json">

let densityCids =
    DensityList.Load("../density-list-repaired.json")
    |> Array.map _.Cid


let processDensity (cid:int) =
    async {
        let! record = getFile cid
        match record with
        | Some record ->
            return Some (cid, extractionPipeline record)
        | None -> 
            return None
    }

parseDensity "0.853 - 0.859"
parseDensity "1.468 @ 20 °C/4 °C"
parseDensity "0.910 - 0.925 at 59.9 °F (NTP, 1992)"
