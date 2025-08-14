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
    pRangeOrFloat .>> spaces .>>. opt densityUnits .>>. opt (spaces >>. atQuantifier >>. tempOrTempPair) |>> fun ((value, unit), temp) -> 
            { Value = value
              Units = unit
              Temperature = temp }
    

let parseDensity (str:string) : DensityResult option =
    match run densityParser str with
    | Success (data, _, _) ->
        if data.Value <> 0.0 || data.Value = infinity then Some data else None
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with"
        None



// include this format
// 159.46 °C @ 760 MM HG
// 160.00 to 161.00 °C. @ 760.00 mm Hg
// 159.5 °C
// 313-316 °F
// 34.5 °F (NTP, 1992)
// 1.74 °C
// 24.00 to 26.00 °C. @ 760.00 mm Hg
// 171 °F (NTP, 1992)
// 76.6 °C
// 171 °F (77 °C) (closed cup)
// 0.9611 @ 20 °C/4 °C
// 0.96 g/cm³
//Parse failed for: 1.762 at 54 °F (NTP, 1992) - Denser than water; will sink
//Parse failed for: 1.2237 at 77 °F (NTP, 1992) - Denser than water; will sink
//Parse failed for: 1.09 at 68 °F (NTP, 1992) - Denser than water; will sink
//Parse failed for: 0.994 at 72.3 °F (NTP, 1992) - Less dense than water; will float
//Parse failed for: 0.8156 at 61 °F (NTP, 1992) - Less dense than water; will float
//Parse failed for: 1.0497 at 68 °F (USCG, 1999) - Denser than water; will sink
// 1.9 g/cu cm at 20 °C
// Parse failed for: 1.65 g/cu cm at 25 °C

// prev Yield rate 0.8847 -> 0.9395 ->