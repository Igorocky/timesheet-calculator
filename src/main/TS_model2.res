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

let splitTimeByType = (
    ~tsLogRec:tsLogRecord,
    ~regularWorkDurationMinutes:int,
):(int,int,int) => {
    if (tsLogRec.durMinutes == 0) {
        (0,0,0)
    } else if (tsLogRec.date->dateIsWeekend) {
        (0,0,tsLogRec.durMinutes)
    } else if (tsLogRec.durMinutes <= regularWorkDurationMinutes) {
        (tsLogRec.durMinutes,0,0)
    } else {
        let overtimeDurMinutes = tsLogRec.durMinutes - regularWorkDurationMinutes
        (regularWorkDurationMinutes,overtimeDurMinutes,0)
    }
}

let calcAmount = (~ratePerHour:float, ~durMinutes:int):float => {
    ratePerHour *. durMinutes->Belt.Int.toFloat /. 60.0
}

let calcAmountFormula = (~ratePerHour:float, ~durMinutes:int):string => {
    `(${durMinutes->minutesToDurStr}) * ${ratePerHour->floatToCurrencyStr}`
}

let createSumFormula = (parts:array<string>):string => {
    parts->Js_array2.filter(str => str->Js_string2.trim->Js_string2.length > 0)->Js_array2.joinWith(" + ")
}

let tsCalculate = (
    ~tsLog:array<tsLogRecord>,
    ~regularWorkDurationMinutes:int,
    ~regularRatePerHour:float,
    ~overtimeRatePerHour:float,
    ~weekendRatePerHour:float,
) => {
    tsLog->Js_array2.reduce(
        (acc,tsLogRec) => {
            let (regularDur,overtimeDur,weekendDur) = splitTimeByType(~tsLogRec, ~regularWorkDurationMinutes)
            let currCalc = {
                "regularAmount": calcAmount(~ratePerHour=regularRatePerHour, ~durMinutes=regularDur),
                "regularAmountFrm": if (regularDur == 0) {""} else {
                    calcAmountFormula(~ratePerHour=regularRatePerHour, ~durMinutes=regularDur)
                },
                "overtimeAmount": calcAmount(~ratePerHour=overtimeRatePerHour, ~durMinutes=overtimeDur),
                "overtimeAmountFrm": if (overtimeDur == 0) {""} else {
                    calcAmountFormula(~ratePerHour=overtimeRatePerHour, ~durMinutes=overtimeDur)
                },
                "weekendAmount": calcAmount(~ratePerHour=weekendRatePerHour, ~durMinutes=weekendDur),
                "weekendAmountFrm": if (weekendDur == 0) {""} else {
                    calcAmountFormula(~ratePerHour=weekendRatePerHour, ~durMinutes=weekendDur)
                },
            }
            let prev = if (acc->Js_array2.length == 0) {
                {
                    "amountSum":0.,
                    "regularDurMinutesSum":0,
                    "regularAmountSum":0.,
                    "overtimeDurMinutesSum":0,
                    "overtimeAmountSum":0.,
                    "weekendDurMinutesSum":0,
                    "weekendAmountSum":0.,
                }
            } else {
                let prev = acc[acc->Js_array2.length-1]
                {
                    "amountSum":prev.amountSum,
                    "regularDurMinutesSum":prev.regularDurMinutesSum,
                    "regularAmountSum":prev.regularAmountSum,
                    "overtimeDurMinutesSum":prev.overtimeDurMinutesSum,
                    "overtimeAmountSum":prev.overtimeAmountSum,
                    "weekendDurMinutesSum":prev.weekendDurMinutesSum,
                    "weekendAmountSum":prev.weekendAmountSum,
                }
            }
            let amount = currCalc["regularAmount"] +. currCalc["overtimeAmount"] +. currCalc["weekendAmount"]
            acc->Js_array2.push(
                {
                    date: tsLogRec.date,
                    amount,
                    amountSum: prev["amountSum"] +. amount,
                    amountFormula: createSumFormula([
                        currCalc["regularAmountFrm"], currCalc["overtimeAmountFrm"], currCalc["weekendAmountFrm"]
                    ]),
                    regularDurMinutesSum: prev["regularDurMinutesSum"] + regularDur,
                    regularAmountSum: prev["regularAmountSum"] +. currCalc["regularAmount"],
                    overtimeDurMinutesSum: prev["overtimeDurMinutesSum"] + overtimeDur,
                    overtimeAmountSum: prev["overtimeAmountSum"] +. currCalc["overtimeAmount"],
                    weekendDurMinutesSum: prev["weekendDurMinutesSum"] + weekendDur,
                    weekendAmountSum: prev["weekendAmountSum"] +. currCalc["weekendAmount"],
                }
            )->ignore
            acc
        },
        []
    )
}
