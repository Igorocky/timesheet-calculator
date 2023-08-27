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

let classifyTime = ( 
    ~tsLogRec:tsLogRecord, 
    ~regularWorkDurationMinutes:int,
    ~regularRatePerHour:float,
    ~overtimeRatePerHour:float,
    ~weekendRatePerHour:float,
):array<timeType> => {
    if (tsLogRec.durMinutes == 0) {
        []
    } else if (tsLogRec.date->dateIsWeekend) {
        [
            Weekend({
                durMinutes:tsLogRec.durMinutes,
                amount:calcAmount(~ratePerHour=weekendRatePerHour, ~durMinutes=tsLogRec.durMinutes),
                formula:calcAmountFormula(~ratePerHour=weekendRatePerHour, ~durMinutes=tsLogRec.durMinutes),
            })
        ]
    } else if (tsLogRec.durMinutes <= regularWorkDurationMinutes) {
        [
            Regular({
                durMinutes:tsLogRec.durMinutes,
                amount:calcAmount(~ratePerHour=regularRatePerHour, ~durMinutes=tsLogRec.durMinutes),
                formula:calcAmountFormula(~ratePerHour=regularRatePerHour, ~durMinutes=tsLogRec.durMinutes),
            })
        ]
    } else {
        let overtimeDurMinutes = tsLogRec.durMinutes - regularWorkDurationMinutes
        [
            Regular({
                durMinutes: regularWorkDurationMinutes,
                amount:calcAmount(~ratePerHour=regularRatePerHour, ~durMinutes=regularWorkDurationMinutes),
                formula:calcAmountFormula(~ratePerHour=regularRatePerHour, ~durMinutes=regularWorkDurationMinutes),
            }),
            Overtime({
                durMinutes: overtimeDurMinutes,
                amount:calcAmount(~ratePerHour=overtimeRatePerHour, ~durMinutes=overtimeDurMinutes),
                formula:calcAmountFormula(~ratePerHour=overtimeRatePerHour, ~durMinutes=overtimeDurMinutes),
            }),
        ]
    }
}

let calcAmountForTimes = ( times:array<timeType> ):(float,string) => {
    times->Js_array2.reduce(
        ((amount,formula), time) => {
            let (nextAmount,nextFormula) = switch time {
                | Regular({amount,formula}) | Overtime({amount,formula}) | Weekend({amount,formula}) => (amount,formula)
            }
            if (formula == "") {
                (nextAmount,nextFormula)
            } else {
                (amount +. nextAmount, formula ++ " + " ++ nextFormula)
            }
        },
        (0.0, "")
    )
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
    let times = classifyTime(
        ~tsLogRec, 
        ~regularWorkDurationMinutes,
        ~regularRatePerHour,
        ~overtimeRatePerHour,
        ~weekendRatePerHour,
    )
    let (amount, formula) = calcAmountForTimes( times )
    {
        times,
        amount,
        formula: if (formula == "") {""} else {`${floatToCurrencyStr(amount)} = ` ++ formula},
        sum: prevSum +. amount
    }
}

let tsCalculate = (
    ~tsLog:array<tsLogRecord>,
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

let sumTimesByType = (times:array<timeType>):((int,float),(int,float),(int,float)) => {
    let regularDur = ref(0)
    let regularMnt = ref(0.0)
    let overtimeDur = ref(0)
    let overtimeMnt = ref(0.0)
    let weekendDur = ref(0)
    let weekendMnt = ref(0.0)
    times->Js_array2.forEach(time => {
        switch time {
            | Regular({durMinutes,amount}) => {
                regularDur := regularDur.contents + durMinutes
                regularMnt := regularMnt.contents +. amount
            }
            | Overtime({durMinutes,amount}) => {
                overtimeDur := overtimeDur.contents + durMinutes
                overtimeMnt := overtimeMnt.contents +. amount
            }
            | Weekend({durMinutes,amount}) => {
                weekendDur := weekendDur.contents + durMinutes
                weekendMnt := weekendMnt.contents +. amount
            }
        }
    })
    ( 
        (regularDur.contents, regularMnt.contents ),
        (overtimeDur.contents, overtimeMnt.contents ),
        (weekendDur.contents, weekendMnt.contents ),
    )
}

let sumTimesByTypeForLog = (tsLog:array<(tsLogRecord,tsCalc)>):((int,float),(int,float),(int,float)) => {
    tsLog->Js.Array2.map(((_,tsCalc)) => tsCalc.times)->Belt_Array.concatMany->sumTimesByType
}