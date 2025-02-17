module DensityParser
open ParserTemplate
open FParsec


type DensityResult = {
    Value: float
    Units: (Units * Units) option
    Temperature: Temperature option
}

let private densityUnits =
    weightUnits .>> spaces .>> pchar '/' .>> spaces .>>. volumeUnits


let private densityParser =
    pRangeOrFloat .>> spaces .>>. opt densityUnits .>>. opt (spaces >>. atQuantifier >>. temp) |>> fun ((value, unit), temp) -> 
            { Value = value
              Units = unit
              Temperature = temp }
    

let parseDensity (str:string) : DensityResult option =
    match run densityParser str with
    | Success (data, _, _) ->
        if data.Value <> 0.0 then Some data else None
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with: {msg}"
        None


// include this format
//Parse failed for: 1.762 at 54 °F (NTP, 1992) - Denser than water; will sink
//Parse failed for: 1.2237 at 77 °F (NTP, 1992) - Denser than water; will sink
//Parse failed for: 1.09 at 68 °F (NTP, 1992) - Denser than water; will sink
//Parse failed for: 0.994 at 72.3 °F (NTP, 1992) - Less dense than water; will float
//Parse failed for: 0.8156 at 61 °F (NTP, 1992) - Less dense than water; will float
//Parse failed for: 1.0497 at 68 °F (USCG, 1999) - Denser than water; will sink