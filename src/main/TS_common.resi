type date = {
    year: int, 
    month: int, 
    day: int
}

let multilineTextToNonEmptyLines: string => array<string>
let getSpaceSeparatedValuesAsArray: string => array<string>
let rightPad: (~content:string, ~char:string, ~totalLen:int) => string
let leftPad: (~content:string, ~char:string, ~totalLen:int) => string
let floatParse: string => option<float>

let dateIsWeekend: date => bool
let dateToString: date => string
let minutesToDurStr: int => string
let floatToCurrencyStr: float => string


let rndStaticTable: (~header:array<string>, ~data:array<array<string>>) => React.element