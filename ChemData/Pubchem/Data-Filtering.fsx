#r "nuget:FParsec"
#r "nuget:FSharp.Data"
#r "nuget:FSharp.Stats"
#r "nuget:Plotly.NET"
#r "nuget:Cytoscape.NET"
#r "nuget: Newtonsoft.Json"

#r "nuget: DynamicObj, 2.0.0"
#r "nuget: Giraffe.ViewEngine, 1.4.0"

open Cytoscape.NET

open FSharp.Data
open FSharp.Stats
open FParsec
open System.IO
open Plotly.NET
open Plotly.NET.ConfigObjects
open System
open Giraffe.ViewEngine


type FullCompoundType = JsonProvider<"C:\\Users\\jona4\\source\\repos\\Pubchem-Extractor\\Pubchem-Extractor\\PubChem-FullRecord.json">


let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg




let loadData id =
    File.ReadAllText($"C:\\Users\\jona4\\Documents\\ChemData\\{id}.json")

let parseInput (str:string) : float option =
    let tempC =
        pint32 >>. spaces >>. pchar '°' >>. spaces >>. pstring "C"

    let gPerCubic =
        (pstring "g" <|> pstring "kg") >>. spaces >>. pchar '/' >>. spaces >>. choice [
            attempt (pstring "cm³")
            attempt (pstring "cm" >>. spaces >>. pstring "3")
            attempt (pstring "cm" >>. spaces >>. pstring "cu")
            attempt (pstring "l")
        ]

    let meanOfTuple ((x,y): float * float) =
        (x + y) / (2.0)

    let eol =
        spaces .>> eof

    let pRangeOfFloat = choice [
        attempt (pfloat .>> notFollowedBy (pchar '-'))
        attempt ((spaces >>. pfloat .>> spaces .>> skipChar '-' .>> spaces .>>. pfloat .>> followedBy spaces) |>> meanOfTuple)
    ]
    let skipTempUnit =
        skipMany (tempC <|> gPerCubic)


    let s = spaces >>. choice [
        attempt (pRangeOfFloat .>> followedBy eof)
        attempt (pRangeOfFloat .>> spaces .>> skipTempUnit .>> eol)
        attempt (pRangeOfFloat .>> spaces .>> skipTempUnit .>> skipMany anyChar)
        attempt (pfloat .>> spaces .>> pstring "to" .>> spaces .>>. pfloat .>> spaces .>> skipTempUnit .>> eol |>> meanOfTuple)
        attempt (skipManyTill anyChar pRangeOfFloat >>. pRangeOfFloat .>> skipTempUnit .>> eol)
    ]

    match run s str with
    | Success (data,_,_) -> 
        printfn "Input: %s Output: %f" str data
        if data = 0.0 || isNan data then
            None
        else
            Some data
    | Failure (msg,err,state) ->
        printfn "%s" msg
        None

parseInput "0.92-0.94"
parseInput "0.42-0.95 g/cm cu"
parseInput "0.56 to 0.65 g/cm3"
parseInput "1.5-2.5 g/cm3 (metal)"



let getDensity str =
    let ci = FullCompoundType.Parse str
    ci.Record.Section
    |> Array.find (fun x -> x.TocHeading = "Chemical and Physical Properties")
    |>  _.Section
    |> Array.tryFind (fun x -> 
        x.TocHeading = "Experimental Properties")
    |> fun x -> 
        match x with 
        | Some s -> 
            s.Section
            |> Array.tryFind (fun x -> x.TocHeading = "Density")
            |> fun x ->
                match x with
                | Some sec -> 
                    sec.Information
                    |> Array.map (fun x -> 
                        match x.Value.StringWithMarkup |> Array.tryHead with
                        | Some data -> 
                            data.String.String
                        | None -> None
                    )
                    |> Array.choose (fun x -> 
                        match x with
                        | Some x -> parseInput x
                        | None -> None)
                    |> meanTruncated 0.2
                    |> fun x -> 
                        printfn "ID: %d Value: %f" ci.Record.RecordNumber x 
                        Some x
                | None -> None
        | None -> None

        
type Datafile = JsonProvider<"C:\\Users\\jona4\\Desktop\\Facharbeit\\compoundedData.json">
let data = File.ReadAllText("C:\\Users\\jona4\\Desktop\\Facharbeit\\compoundedData.json") |> Datafile.Parse
let sortedData = [for x in data -> (x.Item1,x.Item2)] |> List.sortBy(fun (id,den) -> den) |> List.where(fun (id,den) -> den < 4.0m)


let scale = 0.1m

let groupedData = sortedData |> List.groupBy(fun (x,y) -> Math.Floor(y / scale))


let keysData = groupedData |> List.map(fun (key,y) -> scale * (key - 1m))
let values = groupedData |> List.map(fun (key,y) -> y.Length)
let textData = keysData |> List.map(fun x -> string x)

open Elements




let chartConfig =
    Config.init(
        ToImageButtonOptions = 
            ToImageButtonOptions.init(
                Format = StyleParam.ImageFormat.SVG,
                Filename = "",
                Width = 1000.,
                Height = 1000.,
                Scale = 10.
            )
    )



Chart.Column(values = values, Keys = keysData) 
    |> Chart.withConfig chartConfig
    |> Chart.withTemplate ChartTemplates.transparent
    |> Chart.withXAxisStyle (TitleText="Dichte in g/cm3", MinMax = (0, 2), ShowGrid = true)
    |> Chart.withYAxisStyle (TitleText="#Moleküle", ShowGrid = true)
    |> Chart.show

