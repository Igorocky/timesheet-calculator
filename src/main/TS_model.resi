open TS_common

type tsLogRecord = {
    date: date,
    durMinutes: int,
}

type tsCalc = {
    date: date,
    amount:float,
    amountSum:float,
    amountFormula:string,
    regularDurMinutesSum:int,
    regularAmountSum:float,
    overtimeDurMinutesSum:int,
    overtimeAmountSum:float,
    weekendDurMinutesSum:int,
    weekendAmountSum:float,
}

let tsCalculate: (
    ~tsLog:array<tsLogRecord>,
    ~regularWorkDurationMinutes:int,
    ~regularRatePerHour:float,
    ~overtimeRatePerHour:float,
    ~weekendRatePerHour:float,
) => array<tsCalc>
