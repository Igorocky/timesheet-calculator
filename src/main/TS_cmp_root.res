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
    tsLog:option<array<(tsLogRecord,tsCalc)>>,
}

let makeInitialState = () => {
    {
        tsLogText:"",
        tsLog:None,
    }
}

@warning("-27")
let setLogText = (st, str) => {
    {
        tsLogText:str,
        tsLog:None,
    }
}

let setLog = (st,arrOpt) => {
    {
        ...st,
        tsLog:arrOpt,
    }
}

let getLocStorKey = subKey => {
    "timesheets." ++ subKey
}

@react.component
let make = () => {
    let modalRef = useModalRef()
    let (state, setState) = React.useState(makeInitialState)

    let (regularWorkDurationHrsStr, setRegularWorkDurationHrsStr) = useStateFromLocalStorageStr(
        ~key=getLocStorKey("regularWorkDurationHrsStr"), ~default=""
    )
    let (regularWorkDurationHrsStrErr, setRegularWorkDurationHrsStrErr) = React.useState(() => false)

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

    let actRegularWorkDurationHrsStrChanged = str => {
        setRegularWorkDurationHrsStr(_ => str)
        setRegularWorkDurationHrsStrErr(_ => false)
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
    
    let actTsLogChanged = arrOpt => {
        setState(setLog(_, arrOpt))
    }

    let actCalculate = () => {
        switch state.tsLogText->parseTimesheet {
            | Error(msg) => openInfoDialog(~modalRef, ~text=`Error: ${msg}`, ())
            | Ok(tsLog) => {
                if (tsLog->Js_array2.length == 0) {
                    openInfoDialog(~modalRef, ~text=`The timesheet is empty.`, ())
                } else {
                    let regularWorkDurationHrsOpt = regularWorkDurationHrsStr->floatParse
                    let regularRatePerHourOpt = regularRatePerHourStr->floatParse
                    let overtimeRatePerHourOpt = overtimeRatePerHourStr->floatParse
                    let weekendRatePerHourOpt = weekendRatePerHourStr->floatParse
                    
                    let hasErr = ref(false)
                    if (regularWorkDurationHrsOpt->Belt.Option.isNone) {
                        setRegularWorkDurationHrsStrErr(_ => true)
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
                        actTsLogChanged(Some(
                            tsCalculate(
                                ~tsLog,
                                ~prevSum=0.0,
                                ~regularWorkDurationHrs=regularWorkDurationHrsOpt->Belt.Option.getExn,
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
        switch state.tsLog {
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
        let display = switch state.tsLog {
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
                    ~name="Regular work duration, hours", 
                    ~value=regularWorkDurationHrsStr, 
                    ~onChange=actRegularWorkDurationHrsStrChanged,
                    ~error=regularWorkDurationHrsStrErr,
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

    let rndLogRecordHeader = () => {
        <tr>
            <th className="table-single-border timesheet-cell">
                {React.string("Date")}
            </th>
            <th className="table-single-border timesheet-cell">
                {React.string("Type of day")}
            </th>
            <th className="table-single-border timesheet-cell">
                {React.string("Amount")}
            </th>
            <th className="table-single-border timesheet-cell">
                {React.string("Sum")}
            </th>
            <th className="table-single-border timesheet-cell">
                {React.string("Calculation")}
            </th>
        </tr>
    }

    let rndLogRecord = (~id:int, ~logRec:tsLogRecord, ~calcData:tsCalc) => {
        <tr key={id->Belt_Int.toString} className="highlighted-on-hover">
            <td className="table-single-border timesheet-cell">
                {logRec.date->dateToString->React.string}
            </td>
            <td className="table-single-border timesheet-cell">
                {
                    if (logRec.date->dateIsWeekend) {
                        React.string("weekend")
                    } else {
                        React.null
                    }
                }
            </td>
            <td className="table-single-border timesheet-cell">
                {calcData.amount->floatToCurrencyStr->React.string}
            </td>
            <td className="table-single-border timesheet-cell">
                {calcData.sum->floatToCurrencyStr->React.string}
            </td>
            <td className="table-single-border timesheet-cell">
                {calcData.formula->React.string}
            </td>
        </tr>
    }

    let rndContent = () => {
        switch state.tsLog {
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
            | Some(tsLog) => {
                <Col>
                    <Button onClick={_=>actTsLogChanged(None)} variant=#contained > {React.string("Edit")} </Button>
                    {rndParams()}
                    <table 
                        style=ReactDOM.Style.make(
                            ~borderCollapse="collapse", 
                            ~border="none", 
                            ~padding="5px",
                            ()
                        )
                    >
                        <thead>
                            {rndLogRecordHeader()}
                        </thead>
                        <tbody>
                            {
                                tsLog->Js.Array2.mapi(((logRec,calcData),i) => {
                                    rndLogRecord(~id=i, ~logRec, ~calcData)
                                })->React.array
                            }
                        </tbody>
                    </table>
                </Col>
            }
        }
    }

    <Col style=ReactDOM.Style.make(~padding="15px", ())>
        {rndContent()}
        <Expln_React_Modal modalRef />
    </Col>
    

}