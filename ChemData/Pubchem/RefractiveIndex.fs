module RefractiveIndex
open ParserTemplate
open FParsec
// 1.445-1.451
type RefractiveIndexResult = {
    Value:float
}

let private refractiveIndexParser =
    pRangeOrFloat


let parseRefractiveIndex (str:string) =
    match run refractiveIndexParser str with
    | Success (data, _, _) ->
        Some data
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with: "
        None