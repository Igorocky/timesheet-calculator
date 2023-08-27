type date = {
    year: int, 
    month: int, 
    day: int
}

type tsLogRecord = {
    date: date,
    durMinutes: int,
}

type timeType =
    | Regular({durMinutes:int})
    | Overtime({durMinutes:int})
    | Weekend({durMinutes:int})

type tsCalc = {
    times:array<timeType>,
    amount:float,
    formula:string,
    sum:float,
}

let dateIsWeekend: date => bool
let dateToString: date => string
let minutesToDurStr: int => string

let tsCalculateLogRec: (
    ~tsLogRec:tsLogRecord,
    ~prevSum:float,
    ~regularWorkDurationHrs:float,
    ~regularRatePerHour:float,
    ~overtimeRatePerHour:float,
    ~weekendRatePerHour:float,
) => tsCalc

let tsCalculate: (
    ~tsLog:array<tsLogRecord>,
    ~regularWorkDurationHrs:float,
    ~regularRatePerHour:float,
    ~overtimeRatePerHour:float,
    ~weekendRatePerHour:float,
) => array<(tsLogRecord,tsCalc)>