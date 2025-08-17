module KovatsRetentionParser

open ParserTemplate
open FParsec


type ColumnType =
    | StandardNonPolar
    | SemiStandardNonPolar
    | StandardPolar

type KovatsRetentionResult = {
    ColumnType: ColumnType
    RI: float array
}

let private columnTypeParser =
    choice [
        attempt (pstringCI "Standard non-polar" >>% StandardNonPolar)
        attempt (pstringCI "Semi-standard non-polar" >>% SemiStandardNonPolar)
        attempt (pstringCI "Standard polar" >>% StandardPolar)
    ]

let identifyColumnType (str:string) =
    match run columnTypeParser str with
    | Success (data, _, _) ->
        Some data
    | Failure (msg, _, _) ->
        printfn $"Column type parse failed for: {str} with {msg}"
        None

