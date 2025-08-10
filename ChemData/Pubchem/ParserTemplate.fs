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
    | CubicKilometer
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
    | HectoPascal
    | KiloPascal
    | Bar
    | Atm

let floatOrInt : Parser<float,unit> =
    pfloat <|> (pint32 |>> float)

let tempC =
    floatOrInt .>> spaces .>> pchar '°' .>> spaces .>> pstring "C" |>> Celsius

let tempK =
    floatOrInt .>> spaces .>> pchar 'K' |>> Kelvin

let tempF =
    floatOrInt .>> spaces .>> pchar '°' .>> spaces .>> pchar 'F' |>> Fahrenheit

let temp = choice [
    attempt tempC
    attempt tempK
    attempt tempF
]


let weightUnits : Parser<Units,unit> =
    spaces >>. choice [
        // Mass
        pstringCI "t" >>% Ton
        pstringCI "kg" >>% Kilogram
        pstringCI "g" >>% Gram
        pstringCI "mg" >>% Milligram

        pstringCI "µg" >>% Microgram
        pstringCI "ug" >>% Microgram
        pstringCI "mcg" >>% Microgram
        
        pstringCI "ng" >>% Nanogram
    ]


let distanceUnits : Parser<Units,unit> =
    spaces >>. choice [
        // Metric distance
        pstringCI "km" >>% Kilometer
        pstring "m" >>% Meter
        pstring "dm" >>% Decimeter
        pstring "cm" >>% Centimeter
        pstring "mm" >>% Millimeter
    ]

let areaUnits : Parser<Units,unit> =
    spaces >>. choice [
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

        pstringCI "mm²" >>% SquareMillimeter
        pstringCI "mm2" >>% SquareMillimeter
        pstringCI "mm^2" >>% SquareMillimeter
    ]

let volumeUnits : Parser<Units,unit> =
    spaces >>. choice [
        // Metric Volume
        pstringCI "m³" >>% CubicMeter 
        pstringCI "m^3" >>% CubicMeter 
        pstringCI "m3" >>% CubicMeter
        pstringCI "cu" >>. spaces >>. pstringCI "m" >>% CubicMeter
        
        pstringCI "dm³" >>% Decimeter 
        pstringCI "dm^3" >>% Decimeter 
        pstringCI "dm3" >>% Decimeter
        pstringCI "cu" >>. spaces >>. pstringCI "dm" >>% Decimeter

        pstringCI "cm³" >>% CubicCentimeter
        pstringCI "cm3" >>% CubicCentimeter
        pstringCI "cm^3" >>% CubicCentimeter
        pstringCI "cu" >>. spaces >>. pstringCI "cm" >>% CubicCentimeter
        pstringCI "cc" >>% CubicCentimeter
        pstringCI "c.c." >>% CubicCentimeter

        pstringCI "mm³" >>% CubicMillimeter 
        pstringCI "mm^3" >>% CubicMillimeter 
        pstringCI "mm3" >>% CubicMillimeter
        pstringCI "cu" >>. spaces >>. pstringCI "mm" >>% CubicMillimeter

        pstringCI "l" >>% Liter
        pstringCI "dl" >>% Deciliter
        pstringCI "cl" >>% Centiliter
        pstringCI "ml" >>% Milliliter
    ]


let temporalUnits : Parser<Units,unit> =
    spaces >>. choice [

        // Seconds
        pstringCI "s" >>% Second

        pstringCI "s^2" >>% SecondSquared
        pstringCI "s2" >>% SecondSquared
    ]


let pressureUnits : Parser<Units,unit> =
    spaces >>. choice [
        pstringCI "mm" .>> spaces .>> pstringCI "Hg" >>%  MMHg
        pstringCI "mmHg" >>% MMHg

        pstringCI "Pa" >>% Pascal
        pstringCI "hPa" >>% HectoPascal
        pstringCI "kPa" >>% KiloPascal
        pstringCI "bar" >>% Bar
        pstringCI "atm" >>% Atm
    ]


let unitParser : Parser<Units,unit> =
    choice [
        distanceUnits
        areaUnits
        volumeUnits      
        weightUnits
        temporalUnits
        pressureUnits
    ]

let meanOfTuple ((x,y): float * float) = (x + y) / 2.0

let eol : Parser<unit,unit> = spaces .>> eof


let pRangeIndicators = 
    choice [
        pstring "to"
        pstring "-"
    ]
    

let atQuantifier : Parser<unit,unit> = 
    spaces .>> choice [
        
        pstring "@" .>> spaces 
        pstringCI "at" .>> spaces
    ]

let tempPair =
    temp .>> spaces .>> pchar '/' .>> spaces .>>. temp |>> fun temps -> Pair(temps)


let tempOrTempPair =
    choice [
        attempt (temp)
        attempt (tempPair)
    ]


let pRangeOrFloat = choice [
    attempt (floatOrInt .>> spaces .>> notFollowedBy (pRangeIndicators))
    attempt (floatOrInt .>> spaces .>> (skipMany pRangeIndicators) .>> spaces .>>. floatOrInt |>> meanOfTuple)
]


