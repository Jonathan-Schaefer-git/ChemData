module BoilingPointParser
open ParserTemplate
open FParsec



type BoilingPointResult = {
    Temperature: Temperature
    Pressure: float option
    Unit: Units option
}


let private boilingPointParser =
    temp .>>. opt (spaces >>. atQuantifier >>. spaces >>. pRangeOrFloat .>> spaces .>>. pressureUnits) |>> fun (temp, (pressure)) ->
        match pressure with
        | Some (pressure, pressureUnit) -> 
            { Temperature = temp
              Pressure = Some pressure
              Unit = Some pressureUnit }
        | None ->
            { Temperature = temp
              Pressure = None
              Unit = None }



let parseBoilingPoint (str:string) : BoilingPointResult option =
    match run boilingPointParser str with
    | Success (data, _, _) ->
        Some data
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with: {msg}"
        None


//parseBoilingPoint "415 °F at 760 mmHg (NTP, 1992)"
//parseBoilingPoint "204.00 to 208.00 °C. @ 760.00 mm Hg"
//parseBoilingPoint "213.5 °C"
//parseBoilingPoint "83.00 to 84.00 °C @ 760.00 mm Hg"