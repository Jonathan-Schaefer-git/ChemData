module ParserTemplate
open FParsec

type Temperature = 
    | Pair of Temperature * Temperature
    | Celsius of float
    | Kelvin of float
    | Fahrenheit of float


type Units =
    | Ton
    | Kilogram
    | Gram
    | Milligram
    | Microgram
    | Nanogram
    | Kilometer
    | SquareKilometer
    | Meter
    | SquareMeter
    | CubicMeter
    | Decimeter
    | SquareDecimeter
    | CubicDecimeter
    | Centimeter
    | SquareCentimeter
    | CubicCentimeter
    | Millimeter
    | SquareMillimeter
    | CubicMillimeter
    | Liter
    | Deciliter
    | Centiliter
    | Milliliter
    | Second
    | SecondSquared
    | MMHg
    | Pascal
    | MilliPascal
    | HectoPascal
    | KiloPascal
    | Bar
    | Atm
    | Poise
    | CentiPoise
    | CentiStoke

let divBy : Parser<unit,unit> =
    spaces >>. pchar '/' >>. spaces

let floatOrInt : Parser<float,unit> =
    pfloat <|> (pint32 |>> float)


let tempUnits = 
    attempt (choice [
        attempt (pchar '°' .>> spaces .>> pstring "C" >>% Celsius)
        attempt (pchar 'K' >>% Kelvin)
        attempt (pchar '°' .>> spaces .>> pchar 'F' >>% Fahrenheit)
    ])

let temp = choice [
    floatOrInt .>> spaces .>>. tempUnits |>> fun (x, f) -> f x
]


let weightUnits : Parser<Units,unit> =
    spaces >>. attempt (choice [
        // Mass
        pstringCI "t" >>% Ton
        pstringCI "kg" >>% Kilogram
        pstringCI "g" >>% Gram
        pstringCI "mg" >>% Milligram

        pstringCI "µg" >>% Microgram
        pstringCI "ug" >>% Microgram
        pstringCI "mcg" >>% Microgram
        
        pstringCI "ng" >>% Nanogram
    ])


let distanceUnits : Parser<Units,unit> =
    spaces >>. attempt (choice [
        // Metric distance
        pstringCI "km" >>% Kilometer
        pstring "m" >>% Meter


        pstring "dm" >>% Decimeter
        pstring "cm" >>% Centimeter
        pstring "mm" >>% Millimeter
    ])

let squareMillimeter =
    attempt (choice [
        pstringCI "mm²" >>% SquareMillimeter
        pstringCI "mm2" >>% SquareMillimeter
        pstringCI "mm^2" >>% SquareMillimeter
    ])

let areaUnits : Parser<Units,unit> =
    spaces >>. attempt (choice [
        // Metric area
        pstringCI "m²" >>% SquareMeter
        pstringCI "m2" >>% SquareMeter
        pstringCI "m^2" >>% SquareMeter
        
        pstringCI "dm²" >>% SquareDecimeter
        pstringCI "dm2" >>% SquareDecimeter
        pstringCI "dm^2" >>% SquareDecimeter

        pstringCI "cm²" >>% SquareCentimeter
        pstringCI "cm2" >>% SquareCentimeter
        pstringCI "cm^2" >>% SquareCentimeter

        squareMillimeter
    ])

let volumeUnits : Parser<Units,unit> =
    spaces >>. attempt (choice [
        // Metric Volume
        pstringCI "m³" >>% CubicMeter 
        pstringCI "m^3" >>% CubicMeter 
        pstringCI "m3" >>% CubicMeter
        attempt (pstringCI "cu" >>. spaces >>. pstringCI "m" >>% CubicMeter)
        
        pstringCI "dm³" >>% Decimeter 
        pstringCI "dm^3" >>% Decimeter 
        pstringCI "dm3" >>% Decimeter
        attempt (pstringCI "cu" >>. spaces >>. pstringCI "dm" >>% Decimeter)

        pstringCI "cm³" >>% CubicCentimeter
        pstringCI "cm3" >>% CubicCentimeter
        pstringCI "cm^3" >>% CubicCentimeter
        attempt (pstringCI "cu" >>. spaces >>. pstringCI "cm" >>% CubicCentimeter)
        pstringCI "cc" >>% CubicCentimeter
        pstringCI "c.c." >>% CubicCentimeter

        pstringCI "mm³" >>% CubicMillimeter 
        pstringCI "mm^3" >>% CubicMillimeter 
        pstringCI "mm3" >>% CubicMillimeter
        attempt (pstringCI "cu" >>. spaces >>. pstringCI "mm" >>% CubicMillimeter)

        pstringCI "l" >>% Liter
        pstringCI "dl" >>% Deciliter
        pstringCI "cl" >>% Centiliter
        pstringCI "ml" >>% Milliliter
    ])


let second =
    spaces >>. pstringCI "s"

let temporalUnits : Parser<Units,unit> =
    spaces >>. attempt (choice [

        // Seconds
        second >>% Second

        pstringCI "s^2" >>% SecondSquared
        pstringCI "s2" >>% SecondSquared
    ])


let pressureUnits : Parser<Units,unit> =
    spaces >>. attempt (choice [
        attempt (pstringCI "mm" .>> spaces .>> pstringCI "Hg" >>%  MMHg)
        pstringCI "mmHg" >>% MMHg
        pstringCI "Pa" >>% Pascal
        pstringCI "hPa" >>% HectoPascal
        pstringCI "kPa" >>% KiloPascal
        pstringCI "bar" >>% Bar
        pstringCI "atm" >>% Atm
    ])


let unitParser : Parser<Units,unit> =
    attempt (choice [
        distanceUnits
        areaUnits
        volumeUnits      
        weightUnits
        temporalUnits
        pressureUnits
    ])

let meanOfTuple ((x,y): float * float) = (x + y) / 2.0

let eol : Parser<unit,unit> = spaces .>> eof


let pRangeIndicators = 
    attempt (choice [
        pstring "to"
        pstring "-"
    ])
    

let atQuantifier : Parser<unit,unit> = 
    spaces .>> attempt (choice [
        
        pstring "@" .>> spaces 
        pstringCI "at" .>> spaces
    ])

let tempPair =
    choice [
        attempt (floatOrInt .>> spaces .>> pRangeIndicators .>> spaces .>>. floatOrInt .>> spaces .>>. opt (tempUnits)
            |>> fun ((temp1, temp2), optUnit) ->
                match optUnit with
                | Some f -> Pair (f temp1, f temp2)
                | None -> Pair(Celsius temp1, Celsius temp2))
        attempt (temp .>> spaces .>> pchar '/' .>> spaces .>>. temp |>> fun temps -> Pair(temps))
        attempt (floatOrInt .>> spaces .>> pchar '±' .>> spaces .>>. floatOrInt .>> spaces .>>. opt (tempUnits) 
            |>> fun ((temp, temp2), optTemp) -> 
                match optTemp with
                | Some f -> Pair (f (temp - temp2), f (temp + temp2))
                | None -> Pair(Celsius(temp - temp2), Celsius(temp + temp2)))
    ]

let tempOrTempPair =
    choice [
        attempt (temp)
        attempt (tempPair)
    ]


let pRangeOrFloat = choice [
    attempt (floatOrInt .>> spaces .>> notFollowedBy pRangeIndicators)
    attempt (floatOrInt .>> spaces .>> skipMany pRangeIndicators .>> spaces .>>. floatOrInt |>> meanOfTuple)
]


