open Expln_React_common
open Expln_React_render
open Expln_React_Mui
open Expln_React_Modal
open Expln_loc_stor_utils
open TS_common
open TS_model
open TS_parser

type state = {
    tsLogText:string,
    tsCalc:option<array<tsCalc>>,
}

let makeInitialState = () => {
    {
        tsLogText:"",
        tsCalc:None,
    }
}

@warning("-27")
let setLogText = (st, str) => {
    {
        tsLogText:str,
        tsCalc:None,
    }
}

let setTsCalc = (st,arrOpt) => {
    {
        ...st,
        tsCalc:arrOpt,
    }
}

let getLocStorKey = subKey => {
    "timesheets." ++ subKey
}

@react.component
let make = () => {
    let modalRef = useModalRef()
    let (state, setState) = React.useState(makeInitialState)

    let (regularWorkDurationMinutesStr, setRegularWorkDurationMinutesStr) = useStateFromLocalStorageStr(
        ~key=getLocStorKey("regularWorkDurationMinutesStr"), ~default=""
    )
    let (regularWorkDurationMinutesStrErr, setRegularWorkDurationMinutesStrErr) = React.useState(() => false)

    let (regularRatePerHourStr, setRegularRatePerHourStr) = useStateFromLocalStorageStr(
        ~key=getLocStorKey("regularRatePerHourStr"), ~default=""
    )
    let (regularRatePerHourStrErr, setRegularRatePerHourStrErr) = React.useState(() => false)

    let (overtimeRatePerHourStr, setOvertimeRatePerHourStr) = useStateFromLocalStorageStr(
        ~key=getLocStorKey("overtimeRatePerHourStr"), ~default=""
    )
    let (overtimeRatePerHourStrErr, setOvertimeRatePerHourStrErr) = React.useState(() => false)

    let (weekendRatePerHourStr, setWeekendRatePerHourStr) = useStateFromLocalStorageStr(
        ~key=getLocStorKey("weekendRatePerHourStr"), ~default=""
    )
    let (weekendRatePerHourStrErr, setWeekendRatePerHourStrErr) = React.useState(() => false)

    let actRegularWorkDurationMinutesStrChanged = str => {
        setRegularWorkDurationMinutesStr(_ => str)
        setRegularWorkDurationMinutesStrErr(_ => false)
    }

    let actRegularRatePerHourStrChanged = str => {
        setRegularRatePerHourStr(_ => str)
        setRegularRatePerHourStrErr(_ => false)
    }

    let actOvertimeRatePerHourStrChanged = str => {
        setOvertimeRatePerHourStr(_ => str)
        setOvertimeRatePerHourStrErr(_ => false)
    }

    let actWeekendRatePerHourStrChanged = str => {
        setWeekendRatePerHourStr(_ => str)
        setWeekendRatePerHourStrErr(_ => false)
    }

    let actTsLogTextChanged = str => {
        setState(setLogText(_, str))
    }
    
    let actTsCalcChanged = arrOpt => {
        setState(setTsCalc(_, arrOpt))
    }

    let actCalculate = () => {
        switch state.tsLogText->parseTimesheet {
            | Error(msg) => openInfoDialog(~modalRef, ~text=`Error: ${msg}`, ())
            | Ok(tsLog) => {
                if (tsLog->Js_array2.length == 0) {
                    openInfoDialog(~modalRef, ~text=`The timesheet is empty.`, ())
                } else {
                    let regularWorkDurationMinutesOpt = regularWorkDurationMinutesStr->durMinutesFromString
                    let regularRatePerHourOpt = regularRatePerHourStr->floatParse
                    let overtimeRatePerHourOpt = overtimeRatePerHourStr->floatParse
                    let weekendRatePerHourOpt = weekendRatePerHourStr->floatParse
                    
                    let hasErr = ref(false)
                    if (regularWorkDurationMinutesOpt->Belt.Option.isNone) {
                        setRegularWorkDurationMinutesStrErr(_ => true)
                        hasErr := true
                    }
                    if (regularRatePerHourOpt->Belt.Option.isNone) {
                        setRegularRatePerHourStrErr(_ => true)
                        hasErr := true
                    }
                    if (overtimeRatePerHourOpt->Belt.Option.isNone) {
                        setOvertimeRatePerHourStrErr(_ => true)
                        hasErr := true
                    }
                    if (weekendRatePerHourOpt->Belt.Option.isNone) {
                        setWeekendRatePerHourStrErr(_ => true)
                        hasErr := true
                    }

                    if (!hasErr.contents) {
                        actTsCalcChanged(Some(
                            tsCalculate(
                                ~tsLog,
                                ~regularWorkDurationMinutes=regularWorkDurationMinutesOpt->Belt.Option.getExn,
                                ~regularRatePerHour=regularRatePerHourOpt->Belt.Option.getExn,
                                ~overtimeRatePerHour=overtimeRatePerHourOpt->Belt.Option.getExn,
                                ~weekendRatePerHour=weekendRatePerHourOpt->Belt.Option.getExn,
                            )
                        ))
                    }
                }
            }
        }
    }

    let rndParam = (~name:string, ~value:string, ~error:bool, ~onChange:string=>unit) => {
        switch state.tsCalc {
            | None => {
                <TextField
                    label=name
                    size=#small
                    style=ReactDOM.Style.make(~width="185px", ())
                    value
                    onChange=evt2str(onChange)
                    error
                />
            }
            | Some(_) => {
                <Row>
                    {React.string(name ++ ": ")}
                    {React.string(value)}
                </Row>
            }
        }
    }

    let rndParams = () => {
        let display = switch state.tsCalc {
            | None => Some("none")
            | Some(_) => None
        }
        let delimeter = 
            <span style=ReactDOM.Style.make(~display?, ())>
                {React.string(" ; ")}
            </span>
        <Row>
            {
                rndParam(
                    ~name="Regular work duration, h:m", 
                    ~value=regularWorkDurationMinutesStr, 
                    ~onChange=actRegularWorkDurationMinutesStrChanged,
                    ~error=regularWorkDurationMinutesStrErr,
                )
            }
            delimeter
            {
                rndParam(
                    ~name="Regular rate per hour", 
                    ~value=regularRatePerHourStr, 
                    ~onChange=actRegularRatePerHourStrChanged,
                    ~error=regularRatePerHourStrErr,
                )
            }
            delimeter
            {
                rndParam(
                    ~name="Overtime rate per hour", 
                    ~value=overtimeRatePerHourStr, 
                    ~onChange=actOvertimeRatePerHourStrChanged,
                    ~error=overtimeRatePerHourStrErr,
                )
            }
            delimeter
            {
                rndParam(
                    ~name="Weekend rate per hour", 
                    ~value=weekendRatePerHourStr, 
                    ~onChange=actWeekendRatePerHourStrChanged,
                    ~error=weekendRatePerHourStrErr,
                )
            }
        </Row>
    }

    let rndContent = () => {
        switch state.tsCalc {
            | None => {
                <Col>
                    {rndParams()}
                    <TextField
                        label="Timesheet data"
                        size=#small
                        style=ReactDOM.Style.make(~width="378px", ())
                        autoFocus=true
                        multiline=true
                        maxRows=10
                        value=state.tsLogText
                        onChange=evt2str(actTsLogTextChanged)
                    />
                    <Button onClick={_=>actCalculate()} variant=#contained > {React.string("Calculate")} </Button>
                </Col>
            }
            | Some(tsCalc) => {
                let last = tsCalc[tsCalc->Js.Array2.length-1]
                <Col>
                    <Button onClick={_=>actTsCalcChanged(None)} variant=#contained > {React.string("Edit")} </Button>
                    {rndParams()}
                    {
                        rndStaticTable(
                            ~header=["Date", "Type of day", "Amount", "Sum", "Calculation"],
                            ~data=tsCalc->Js_array2.map(calcData => {
                                [
                                    calcData.date->dateToString,
                                    if (calcData.date->dateIsWeekend) { "weekend" } else { "" },
                                    calcData.amount->floatToCurrencyStr,
                                    calcData.amountSum->floatToCurrencyStr,
                                    calcData.amountFormula
                                ]
                            })
                        )
                    }
                    {
                        rndStaticTable(
                            ~header=["Work type", "Duration", "Amount"],
                            ~data=[
                                [
                                    "Regular", 
                                    last.regularDurMinutesSum->minutesToDurStr, 
                                    last.regularAmountSum->floatToCurrencyStr
                                ],
                                [
                                    "Over", 
                                    (last.overtimeDurMinutesSum+last.weekendDurMinutesSum)->minutesToDurStr 
                                        ++ ` (overtime ${last.overtimeDurMinutesSum->minutesToDurStr}, ` 
                                        ++ `weekend ${last.weekendDurMinutesSum->minutesToDurStr})`,
                                    (last.overtimeAmountSum +. last.weekendAmountSum)->floatToCurrencyStr
                                        ++ ` (overtime ${last.overtimeAmountSum->floatToCurrencyStr}, ` 
                                        ++ `weekend ${last.weekendAmountSum->floatToCurrencyStr})`,
                                ],
                            ]
                        )
                    }
                    {
                        <span>
                            <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                                {React.string(`Total time: `)}
                            </span>
                            <span>
                                {
                                    React.string(
                                        (
                                            last.regularDurMinutesSum
                                            +last.overtimeDurMinutesSum
                                            +last.weekendDurMinutesSum
                                        )->minutesToDurStr
                                    )
                                }
                            </span>
                        </span>
                    }
                    {
                        <span>
                            <span style=ReactDOM.Style.make(~fontWeight="bold", ())>
                                {React.string(`Total amount: `)}
                            </span>
                            <span>
                                {
                                    React.string(
                                        (
                                            last.regularAmountSum 
                                            +. last.overtimeAmountSum 
                                            +. last.weekendAmountSum
                                        )->floatToCurrencyStr
                                    )
                                }
                            </span>
                        </span>
                    }
                </Col>
            }
        }
    }

    <Col style=ReactDOM.Style.make(~padding="15px", ())>
        {rndContent()}
        <Expln_React_Modal modalRef />
    </Col>
}