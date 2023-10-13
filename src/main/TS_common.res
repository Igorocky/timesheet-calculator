type date = {
    year: int, 
    month: int, 
    day: int
}

let splitByRegex = (str,regex) => {
    str
        ->Js_string2.splitByRe(regex)
        ->Js_array2.map(strOpt => strOpt->Belt_Option.getWithDefault("")->Js_string2.trim)
        ->Js_array2.filter(str => str->Js_string2.length > 0)
}

let newLineRegex = %re("/[\n\r]/")
let multilineTextToNonEmptyLines = splitByRegex(_, newLineRegex)

let whitespaceDelimRegex = %re("/[\s\n\r]/")
let getSpaceSeparatedValuesAsArray = splitByRegex(_, whitespaceDelimRegex)

let rightPad = (~content:string, ~char:string, ~totalLen:int):string => {
    let contentLen = content->Js_string2.length
    if (totalLen <= contentLen) {
        content
    } else {
        content ++ Js_string2.repeat(char, totalLen - contentLen)
    }
}

let leftPad = (~content:string, ~char:string, ~totalLen:int):string => {
    let contentLen = content->Js_string2.length
    if (totalLen <= contentLen) {
        content
    } else {
        Js_string2.repeat(char, totalLen - contentLen) ++ content
    }
}

let floatToCurrencyStr = (amount:float):string => {
    amount->Js.Float.toFixedWithPrecision(~digits=2)
}

let floatRegex = %re("/^\d+(\.\d+)?$/")
let floatParse = (str:string):option<float> => {
    if (floatRegex->Js.Re.test_(str)) {
        str->Belt_Float.fromString
    } else {
        None
    }
}

let rndStaticHeader = (header:array<string>):React.element => {
    <tr>
        {
            header->Js_array2.mapi((cell,i) => {
                <th className="table-single-border timesheet-cell" key={i->Belt.Int.toString} >
                    {React.string(cell)}
                </th>
            })->React.array
        }
    </tr>
}

let rndStaticRow = (key:int, row:array<string>):React.element => {
    <tr className="highlighted-on-hover" key={key->Belt.Int.toString} >
        {
            row->Js_array2.mapi((cell,i) => {
                <td className="table-single-border timesheet-cell" key={i->Belt.Int.toString} >
                    {React.string(cell)}
                </td>
            })->React.array
        }
    </tr>
}

let rndStaticTable = (~header:array<string>, ~data:array<array<string>>):React.element => {
    <table style=ReactDOM.Style.make( ~borderCollapse="collapse", ~border="none", ~padding="5px", () ) >
        <thead>
            {rndStaticHeader(header)}
        </thead>
        <tbody>
            { data->Js_array2.mapi((row,i) => rndStaticRow(i,row))->React.array }
        </tbody>
    </table>
}

let dateIsWeekend = (date:date):bool => {
    let dayOfWeek = Js.Date.makeWithYMD(
        ~year=date.year->Belt.Int.toFloat, 
        ~month=(date.month-1)->Belt.Int.toFloat, 
        ~date=date.day->Belt.Int.toFloat,
        ()
    )->Js.Date.getDay->Belt.Float.toInt
    dayOfWeek == 0 || dayOfWeek == 6
}

let dateToString = date => {
    leftPad(~content=date.year->Belt_Int.toString, ~char="0", ~totalLen=4)
        ++ "-"
        ++ leftPad(~content=date.month->Belt_Int.toString, ~char="0", ~totalLen=2)
        ++ "-"
        ++ leftPad(~content=date.day->Belt_Int.toString, ~char="0", ~totalLen=2)
}

let minutesToDurStr = (minutes:int):string => {
    let hours = minutes / 60
    let minutes = mod(minutes, 60)
    hours->Belt_Int.toString ++ " hrs " ++ minutes->Belt_Int.toString ++ " min"
}