﻿module MeltingPointParser

open ParserTemplate
open FParsec


type MeltingPointResult = {
    Temperature: Temperature
    Pressure: float option
    Unit: Units option
}

let private meltingPointParser =
    temp |>> fun temp -> { Pressure=None; Temperature = temp; Unit=None }


let parseMeltingPoint (str:string) : MeltingPointResult option =
    match run meltingPointParser str with
    | Success (data, _, _) ->
        Some data
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with: {msg}"
        None