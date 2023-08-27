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

let calcAmount = (~ratePerHour:float, ~durMinutes:int):float => {
    ratePerHour *. durMinutes->Belt.Int.toFloat /. 60.0
}

let calcAmountFormula = (~ratePerHour:float, ~durMinutes:int):string => {
    `(${durMinutes->minutesToDurStr}) * ${ratePerHour->floatToCurrencyStr}`
}

let classifyTime = ( ~tsLogRec:tsLogRecord, ~regularWorkDurationMinutes:int):array<timeType> => {
    if (tsLogRec.durMinutes == 0) {
        []
    } else if (tsLogRec.date->dateIsWeekend) {
        [Weekend({durMinutes:tsLogRec.durMinutes})]
    } else if (tsLogRec.durMinutes <= regularWorkDurationMinutes) {
        [Regular({durMinutes:tsLogRec.durMinutes})]
    } else {
        let overtimeDurMinutes = tsLogRec.durMinutes - regularWorkDurationMinutes
        [
            Regular({durMinutes: regularWorkDurationMinutes}),
            Overtime({durMinutes: overtimeDurMinutes}),
        ]
    }
}

let tsCalculateLogRec = (
    ~tsLogRec:tsLogRecord,
    ~prevSum:float,
    ~regularWorkDurationHrs:float,
    ~regularRatePerHour:float,
    ~overtimeRatePerHour:float,
    ~weekendRatePerHour:float,
):tsCalc => {
    let regularWorkDurationMinutes = (regularWorkDurationHrs *. 60.0)->Belt_Float.toInt
    if (tsLogRec.durMinutes == 0) {
        {
            times: classifyTime(~tsLogRec, ~regularWorkDurationMinutes),
            amount:0.0,
            formula:"",
            sum:prevSum,
        }
    } else if (tsLogRec.date->dateIsWeekend) {
        let amount = calcAmount(~ratePerHour=weekendRatePerHour, ~durMinutes=tsLogRec.durMinutes)
        let sum = prevSum +. amount
        {
            times: classifyTime(~tsLogRec, ~regularWorkDurationMinutes),
            amount,
            formula:`${floatToCurrencyStr(amount)} = ` 
                ++ calcAmountFormula(~ratePerHour=weekendRatePerHour, ~durMinutes=tsLogRec.durMinutes),
            sum,
        }
    } else if (tsLogRec.durMinutes <= regularWorkDurationMinutes) {
        let amount = calcAmount(~ratePerHour=regularRatePerHour, ~durMinutes=tsLogRec.durMinutes)
        let sum = prevSum +. amount
        {
            times: classifyTime(~tsLogRec, ~regularWorkDurationMinutes),
            amount,
            formula:`${floatToCurrencyStr(amount)} = ` 
                ++ calcAmountFormula(~ratePerHour=regularRatePerHour, ~durMinutes=tsLogRec.durMinutes),
            sum,
        }
    } else {
        let overtimeDurMinutes = tsLogRec.durMinutes - regularWorkDurationMinutes
        let amount = calcAmount(~ratePerHour=regularRatePerHour, ~durMinutes=regularWorkDurationMinutes)
            +. calcAmount(~ratePerHour=overtimeRatePerHour, ~durMinutes=overtimeDurMinutes)
        let sum = prevSum +. amount
        {
            times: classifyTime(~tsLogRec, ~regularWorkDurationMinutes),
            amount,
            formula:`${floatToCurrencyStr(amount)} = ` 
                ++ calcAmountFormula(~ratePerHour=regularRatePerHour, ~durMinutes=regularWorkDurationMinutes)
                ++ " + "
                ++ calcAmountFormula(~ratePerHour=overtimeRatePerHour, ~durMinutes=overtimeDurMinutes),
            sum,
        }
    }
}

let tsCalculate = (
    ~tsLog:array<tsLogRecord>,
    ~prevSum:float,
    ~regularWorkDurationHrs:float,
    ~regularRatePerHour:float,
    ~overtimeRatePerHour:float,
    ~weekendRatePerHour:float,
):array<(tsLogRecord,tsCalc)> => {
    tsLog->Js_array2.reduce(
        (res,tsLogRec) => {
            let len = res->Js_array2.length
            let prevSum = if (len == 0) {0.0} else {
                let (_,{sum}) = res[len-1]
                sum
            }
            res->Js_array2.push(
                (
                    tsLogRec,
                    tsCalculateLogRec(
                        ~tsLogRec,
                        ~prevSum,
                        ~regularWorkDurationHrs=regularWorkDurationHrs,
                        ~regularRatePerHour=regularRatePerHour,
                        ~overtimeRatePerHour=overtimeRatePerHour,
                        ~weekendRatePerHour=weekendRatePerHour,
                    )
                )
            )->ignore
            res
        },
        []
    )
}