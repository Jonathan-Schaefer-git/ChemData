#r "nuget:FSharp.Data"
#r "nuget:FParsec"

open System
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


parseInput "0.995"
parseInput "0.9950 g/cu cm at 25 °C"
parseInput "0.917-0.923 g/cm³ at 20°C"
parseInput "1.000 at 277K"
parseInput "1.025-1.029 kg/L at 4°C"


let getDensities (doc:PubChemJSON.Record) =

    getSection "Chemical and Physical Properties" doc
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Density"
    |> _.Information
    |> Array.map(fun x -> printfn "%A" x)


let html = HtmlDocument.Load("https://pubchem.ncbi.nlm.nih.gov/compound/962")

html.Body().DescendantsWithPath(fun x -> x.HasAttribute("id", "Experimental-Properties"))


html.Body().CssSelect("section#Experimental-Properties")

let t = html.CssSelect("section#Experimental-Properties")


let googleUrl = "http://www.google.co.uk/search?q=FSharp.Data"
let doc = HtmlDocument.Load(googleUrl)
