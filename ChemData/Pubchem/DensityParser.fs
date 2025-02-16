module DensityParser
open ParserTemplate
open FParsec


type DensityResult = {
    Value: float
    Units: (Units * Units) option
    Temperature: Temperature option
}


let private densityUnit =
    weightUnits .>> spaces .>> pchar '/' .>> spaces .>>. volumeUnits

let private tempPair = 
    temp .>> opt (spaces >>. pchar '/' >>. spaces >>. temp)

let private pDensity =
    pipe3
        pRangeOrFloat
        (spaces >>. opt densityUnit)
        (spaces >>. opt (tempQuantifier >>. tempPair))
        (fun value unit temp -> 
            { Value = value
              Units = unit
              Temperature = temp })


let private densityParser = 
    spaces >>. choice [
        attempt (pDensity .>> eol)
        attempt (skipManyTill anyChar pDensity >>. pDensity .>> eol)
        attempt (pRangeOrFloat .>> eol |>> fun v -> { Value = v; Units = None; Temperature = None })
        attempt (pDensity .>> pchar '(' .>> skipManyTill anyChar (pchar ')') .>> eol)
        attempt (pDensity .>> pchar '(' .>> skipMany1Till anyChar (pchar ')') .>> skipAnyChar)
    ]


let parseDensity (str:string) : DensityResult option =
    match run densityParser str with
    | Success (data, _, _) ->
        if data.Value <> 0.0 then Some data else None
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str}"
        None