module Pubchem
open System.IO
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
        choice [
            pstring "to"
            pstring "-"
        ]
        
    let pRangeOrFloat = choice [
        attempt (floatOrInt .>> spaces .>> notFollowedBy (pchar '-'))
        attempt (floatOrInt .>> spaces .>> (skipMany pRangeIndicators) .>> spaces .>>. floatOrInt |>> meanOfTuple)
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
            attempt (pDensity .>> pchar '(' .>> skipMany1Till anyChar (pchar ')') .>> skipAnyChar)
        ]

    match run s str with
    | Success (data, _, _) ->
        if data.Value <> 0.0 then Some data else None
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str}"
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


let basePath = System.AppDomain.CurrentDomain.BaseDirectory
let projectRoot = Path.GetFullPath(Path.Combine(basePath, "../../.."))
printfn "Project root: %s" projectRoot


let sdfPath = Path.Combine(projectRoot, "SDF")
let jsonPath = Path.Combine(projectRoot, "JSON-FULL")



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

    







