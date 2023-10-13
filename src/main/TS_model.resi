open TS_common

type tsLogRecord = {
    date: date,
    durMinutes: int,
}

type timeTypeCalcData = {
    durMinutes:int, 
    amount:float, 
    formula:string,
}

type timeType =
    | Regular(timeTypeCalcData)
    | Overtime(timeTypeCalcData)
    | Weekend(timeTypeCalcData)

type tsCalc = {
    times:array<timeType>,
    amount:float,
    formula:string,
    sum:float,
}

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

let sumTimesByTypeForLog: array<(tsLogRecord,tsCalc)> => ((int,float),(int,float),(int,float))