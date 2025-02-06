
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

type DensityResult = {
    Value: float
    Unit: string option
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
        floatOrInt .>>. spaces >>. pchar '°' >>. spaces >>. pstring "C" >>% Celsius

    let tempK =
        floatOrInt .>>. spaces >>. pchar 'K' >>% Kelvin

    let tempF =
        floatOrInt .>>. spaces >>. pchar '°' >>. spaces >>. pchar 'F' >>% Fahrenheit

    let temp = choice [
        attempt (tempC)
        attempt (tempK)
        attempt (tempF)
    ]


    let weightUnit =
        (pstring "g") >>. spaces >>. pchar '/' >>. spaces >>. choice [
            attempt (pstring "cm³")
            attempt (pstring "cm" >>. spaces >>. pstring "3")
            attempt (pstring "cm" >>. spaces >>. pstring "cu")
            attempt (pstring "ml")
        ]

    let meanOfTuple ((x,y): float * float) =
        (x + y) / (2.0)

    let eol =
        spaces .>> eof

    let pRangeOrFloat = choice [
        attempt (floatOrInt .>> notFollowedBy (pchar '-'))
        attempt ((spaces >>. floatOrInt .>> spaces .>> skipChar '-' .>> spaces .>>. floatOrInt .>> followedBy spaces) |>> meanOfTuple)
        attempt ((spaces >>. floatOrInt .>> spaces .>> skipString "to" .>> spaces .>>. floatOrInt .>> followedBy spaces) |>> meanOfTuple)
    ]


    let tempQuantifier = choice [
        pstring "at" >>. spaces
    ]
    


    let s = spaces >>. choice [
        attempt (pRangeOrFloat .>> followedBy eof) |>> fun x -> { Value = x; Temperature = None; Unit = None}
        attempt (pRangeOrFloat .>> spaces .>> weightUnit .>> eol) |>> fun x -> { Value = x; Temperature = None; Unit = Some "g/cm"}
        attempt (pRangeOrFloat .>> spaces .>> opt (weightUnit) .>> spaces .>> tempQuantifier .>>. temp .>> followedBy eol) |>> fun (x,y) -> { Value = x; Temperature = None; Unit = None}
    ]
    
    match run s str with
    | Success (data,_,_) -> 
        printfn "Input: %s Output: %A" str data
        if data.Value <> 0.0 || data.Value = nan then
            None
        else
            Some data
    | Failure (msg,err,state) ->
        printfn "%s" msg
        None


parseInput "0.995"
parseInput "0.9950 g/cu cm at 25 °C"
parseInput "0.917-0.923 g/cm³ at 20°C"
parseInput "1.000 at 277K"
parseInput "1.025-1.029 kg/L at 4°C"
parseInput "Expands on freezing. density: 1.000 g/mL at 3.98°C; 0.999868 at 0°C/4°C"


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
