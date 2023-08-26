open TS_common
open TS_model

let dateRegex = %re("/^(\d{4})-(\d{2})-(\d{2})$/")

let dateFromString = (str:string):option<date> => {
    switch dateRegex->Js.Re.exec_(str) {
        | None => None
        | Some(match) => {
            let captures = match->Js_re.captures
            if (captures->Js.Array2.some(strNull => strNull->Js.Nullable.toOption->Belt_Option.isNone)) {
                None
            } else {
                let partsStr = captures->Js.Array2.map(strNull => strNull->Js.Nullable.toOption->Belt_Option.getExn)
                if (partsStr->Js.Array2.length != 4) {
                    None
                } else {
                    switch partsStr[1]->Belt_Int.fromString {
                        | None => None
                        | Some(year) => {
                            switch partsStr[2]->Belt_Int.fromString {
                                | None => None
                                | Some(month) => {
                                    switch partsStr[3]->Belt_Int.fromString {
                                        | None => None
                                        | Some(day) => Some({year, month, day})
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

let tsLogRecordFromString = (str:string): result<tsLogRecord,string> => {
    let parts = str->getSpaceSeparatedValuesAsArray
    if (parts->Js_array2.length != 3) {
        Error(`Expected 3 values but got ${parts->Js_array2.length->Belt.Int.toString} in the record: '${str}'`)
    } else {
        switch dateFromString(parts[0]) {
            | None => Error(`Cannot parse '${parts[0]}' as date in the record: '${str}'`)
            | Some(date) => {
                switch parts[1]->Belt_Int.fromString {
                    | None => Error(`Cannot parse '${parts[1]}' as hours in the record: '${str}'`)
                    | Some(hours) => {
                        switch parts[2]->Belt_Int.fromString {
                            | None => Error(`Cannot parse '${parts[2]}' as minutes in the record: '${str}'`)
                            | Some(minutes) => {
                                Ok({
                                    date,
                                    durMinutes: hours*60+minutes,
                                })
                            }
                        }
                    }
                }
            }
        }
    }
}

let isHeader = (idx:int,str:string):bool => {
    idx == 0 
        && str->getSpaceSeparatedValuesAsArray->Js.Array2.every( cellData => {
            cellData->dateFromString->Belt.Option.isNone
                && cellData->Belt_Int.fromString->Belt.Option.isNone
        })
}

let parseTimesheet = (str:string):result<array<tsLogRecord>,string> => {
    str->multilineTextToNonEmptyLines
        ->Js.Array2.filteri((line,i) => !isHeader(i,line))
        ->Js.Array2.reduce(
            (res,line) => {
                switch res {
                    | Error(_) => res
                    | Ok(log) => {
                        switch line->tsLogRecordFromString {
                            | Error(msg) => Error(msg)
                            | Ok(logRec) => {
                                log->Js_array2.push(logRec)->ignore
                                res
                            }
                        }
                    }
                }
            },
            Ok([])
        )
}