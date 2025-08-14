module ViscosityParser

open ParserTemplate
open FParsec


type ViscosityResult = {
    Value: float
    Temperature: Temperature option
}

let private viscosityUnit = choice [
    attempt (squareMillimeter .>> divBy .>> second)
]

let private viscosityParser =
    choice [
        attempt (pRangeOrFloat .>>. viscosityUnit .>>. opt (atQuantifier >>. spaces >>. tempOrTempPair)
            |>> fun ((value, unit), temp) -> 
                match unit, temp with
                | SquareMillimeter, Some temp -> {Value = value; Temperature = Some temp }
                | SquareMillimeter, None -> { Value = value; Temperature = None }
                | _, _ -> failwith $"Parsed the wrong area unit for viscosity {unit} instead of mm^2"
            )
    ]


let parseViscosity (str:string) =
    match run viscosityParser str with
    | Success (data, _, _) ->
        Some data
    | Failure (msg, _, _) ->
        printfn $"Parse failed for: {str} with: "
        None
// 24 mm²/s at 25 °C


// mPa
// CentiStoke
// cSt
// cS
// mPa . s
// mPa . sec
// sq mm / s
// pascal-sec pacsal sec
// centipoise
