type date = {
    year: int, 
    month: int, 
    day: int
}

type tsLogRecord = {
    date: date,
    durMinutes: int,
}

type tsCalc = {
    amount:float,
    formula:string,
    sum:float,
}

let dateIsWeekend: date => bool
let dateToString: date => string
let minutesToDurStr: int => string

let tsCalculate: (
    ~tsLogRec:tsLogRecord,
    ~prevSum:float,
    ~regularWorkDurationHrs:float,
    ~regularRatePerHour:float,
    ~overtimeRatePerHour:float,
    ~weekendRatePerHour:float,
) => tsCalc