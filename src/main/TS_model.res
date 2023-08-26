open TS_common

type date = {
    year:int,
    month:int,
    day:int
}

type tsLogRecord = {
    date: date,
    durMinutes: int,
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