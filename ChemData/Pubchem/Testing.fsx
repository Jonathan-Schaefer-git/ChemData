#r "nuget:FSharp.Data"
#r "nuget:FParsec"

open FParsec
open FSharp.Data

type Temperature = 
    | Celsius of float
    | Kelvin of float
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

// 0.995
// 0.9950 g/cu cm at 25 °C
// 0.9950 g/cm^3 at 25 °C
// 1.000 at 277K
let densityParsers (inp:string) =

    let str s = pstring s


    let cubicCm = 
        choice [
            str "g/cu cm" >>% "g/cm^3"
            str "g/cm^3" >>% "g/cm^3"
            str "g/cm3" >>% "g/cm^3"
            str "g/cc" >>% "g/cm^3"
        ]
    
    let unitParser = 
        choice [
            cubicCm
            str "g/mL" >>% "g/mL"
            str "g/ml" >>% "g/mL"
            str "g/L" >>% "g/L"
        ]

    let tempUnit =
        choice [
            str "°C" <|> str "C" >>% (fun v -> Celsius v)
            str "K" >>% (fun v -> Kelvin v)
        ]

    let tempValue =
        pfloat .>>. (spaces >>. tempUnit <|> (lookAhead (noneOf "/;") >>% (fun v -> Unknown(v.ToString()))))
        |>> (fun (v, u) -> u v)


    let tempPrefix =
        choice [
            str "at"
            str "Temp of max density"
            str "temperature"
            str "density at"
        ] .>> spaces

    let temperatureParser =
        opt (tempPrefix >>. spaces) >>. tempValue

    // Main density parser
    let densityValue = 
        pfloat .>>. opt (spaces >>. unitParser)
    
    let rangeSeparator = str "/" <|> str "-"
    
    let densityEntry =
        densityValue .>>. opt (spaces >>. temperatureParser .>>. opt (rangeSeparator >>. temperatureParser))
        |>> fun ((value, unit), temp) ->
            let finalTemp = 
                temp 
                |> Option.map (fun (t1, t2) -> 
                    match t2 with
                    | Some t -> [t1; t]
                    | None -> [t1])
            { Value = value; Unit = unit; Temperature = finalTemp |> Option.map List.head }

    // Full parser to handle multiple entries
    let entries = 
        sepBy densityEntry (skipAnyOf [';'; '.'; ')'; '('] <|> spaces)
        .>> eof

    match run entries inp with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> 
        printfn "Parser failed: %s" error
        []


densityParsers "0.995"

let getDensities (doc:PubChemJSON.Record) =


    getSection doc "Chemical and Physical Properties"
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Density"
    |> _.Information
    |> Array.map(fun x -> x)

    sample.Record.Section
    |> Array.filter(fun x -> x.TocHeading = "Chemical and Physical Properties")
    |> Array.tryHead
    |> function
        | Some (x) ->
            x.Section
            |> Array.filter(fun sec -> sec.TocHeading = "Experimental Properties")
            |> Array.tryHead
            |> function
                | Some x ->
                    x.Section
                    |> Array.filter(fun sec -> sec.TocHeading = "Density")
                    |> Array.tryHead
                    |> function
                        | Some x ->
                            x.
                        | None -> failwith "Doesnt have an associated density record"
                | None _ -> failwith "No experimental properties for this record"
        | None _ -> failwith "No description of chemical and physical properties for this record"

let html = HtmlDocument.Load("https://pubchem.ncbi.nlm.nih.gov/compound/962")

html.Body().DescendantsWithPath(fun x -> x.HasAttribute("id", "Experimental-Properties"))


html.Body().CssSelect("section#Experimental-Properties")

let t = html.CssSelect("section#Experimental-Properties")


let googleUrl = "http://www.google.co.uk/search?q=FSharp.Data"
let doc = HtmlDocument.Load(googleUrl)
