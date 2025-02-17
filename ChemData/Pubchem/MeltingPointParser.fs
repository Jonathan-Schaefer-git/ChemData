module MeltingPointParser

open ParserTemplate
open FParsec


type MeltingPointResult = {
    Temperature: Temperature
}

let private meltingPointParser =
    temp |>> fun temp -> { Temperature = temp }


let parseMeltingPoint (str:string) : MeltingPointResult option =
    match run meltingPointParser str with
    | Success (data, _, _) ->
        Some data
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with: {msg}"
        None