module Pubchem

open NCDK.Smiles
open NCDK.Default

open FSharp.Data
open FParsec



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
    let ws = spaces
    let str s = pstring s
    
    let parseRange = 
        pipe2
            (pfloat .>> ws)
            (pfloat .>> ws)
            (fun a b -> [a; b])
    
    // Enhanced temperature parser
    let tempUnit = choice [
        (str "°C" <|> str "C" >>% Celsius)
        (str "K" >>% Kelvin)
        (restOfLine true >>% Unknown)
    ]
    
    let temperatureParser =
        (pfloat .>> ws .>>. tempUnit)
        |>> function
            | (v, Celsius) -> Celsius v
            | (v, Kelvin) -> Kelvin v
            | (v, Unknown u) -> Unknown $"{v} {u}"
    
    // Enhanced unit parser with normalization
    let unitParser = 
        (str "g" <|> str "kg") .>>. 
        (ws >>. str "/" >>. ws >>. (
            str "cm³" <|> str "cm3" <|> str "cu cm" <|> str "cc" >>% "cm³"
            <|> str "mL" <|> str "ml" >>% "mL"
            <|> str "L" >>% "L"
        )) |>> (fun (mass, vol) -> $"{mass}/{vol}")
    
    // Value parser with range support
    let valueParser =
        attempt (parseRange |>> (fun vs -> vs, true))
        <|> (pfloat |>> (fun v -> [v], false))
    
    // Main density parser
    let densityEntry =
        pipe3
            (valueParser .>> ws)
            (opt (unitParser .>> ws))
            (opt (str "at" >>. ws >>. temperatureParser))
            (fun (values, isRange) unit temp ->
                values |> List.map (fun value ->
                    { Value = value
                      Unit = unit
                      Temperature = temp }
                ))
    // Full parser handling multiple entries
    let entries = 
        many (densityEntry .>> ws .>> skipMany (noneOf ['0'-'9';'.']))
    
    match run (entries .>> eof) inp with
    | Success(result, _, _) -> List.concat result
    | Failure(error, _, _) -> 
        printfn "Parser error: %s" error
        []




let getDensities (doc:PubChemJSON.Record) =


    getSection "Chemical and Physical Properties" doc
    |> getSubSection "Experimental Properties"
    |> getPropertySection "Density"
    |> _.Information
    |> Array.map(fun x -> x)
    |> ignore
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
                            ()
                        | None -> failwith "Doesnt have an associated density record"
                | None _ -> failwith "No experimental properties for this record"
        | None _ -> failwith "No description of chemical and physical properties for this record"

let html = HtmlDocument.Load("https://pubchem.ncbi.nlm.nih.gov/compound/962")

html.Body().DescendantsWithPath(fun x -> x.HasAttribute("id", "Experimental-Properties"))


html.Body().CssSelect("section#Experimental-Properties")


