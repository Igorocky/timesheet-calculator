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

