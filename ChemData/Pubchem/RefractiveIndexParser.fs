module RefractiveIndexParser
open ParserTemplate
open FParsec
// 1.445-1.451
type RefractiveIndexResult = {
    Value:float
    Temperature:Temperature option
}

let private refractionPrelude = choice [
    pchar ':'
    pchar '='
]

let private refractiveEnvironmentParser =
    temp .>> opt(spaces .>> pchar '/' .>> spaces .>> pstringCI "D")

let private refractiveIndexParser =
    opt (pstringCI "Index of refraction" >>. spaces >>. opt (refractionPrelude)) >>. spaces >>. pRangeOrFloat .>> spaces .>>. opt (atQuantifier >>. refractiveEnvironmentParser) 
    |>> fun (value, temp) -> 
        { Value=value; Temperature = temp}


let parseRefractiveIndex (str:string) =
    match run refractiveIndexParser str with
    | Success (data, _, _) ->
        Some data
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with: {msg}"
        None