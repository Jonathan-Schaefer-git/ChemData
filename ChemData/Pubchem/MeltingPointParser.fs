module MeltingPointParser

open ParserTemplate
open FParsec


type MeltingPointResult = {
    Temperature: Temperature
    Pressure: float option
    Unit: Units option
}

let private meltingPointParser =
    tempOrTempPair .>>. opt (spaces >>. atQuantifier >>. spaces >>. pRangeOrFloat .>> spaces .>>. pressureUnits) |>> fun (temp, (pressure)) ->
        match pressure with
        | Some (pressure, pressureUnit) -> 
            { Temperature = temp
              Pressure = Some pressure
              Unit = Some pressureUnit }
        | None ->
            { Temperature = temp
              Pressure = None
              Unit = None }

let parseMeltingPoint (str:string) : MeltingPointResult option =
    match run meltingPointParser str with
    | Success (data, _, _) ->
        Some data
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with: "
        None