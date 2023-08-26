open Expln_React_common
open Expln_React_render
open Expln_React_Mui
open Expln_React_Modal
open Expln_utils_promise
open TS_model
open TS_parser

type state = {
    tsLogText:string,
    tsLog:option<array<tsLogRecord>>,
}

let makeInitialState = () => {
    {
        tsLogText:"",
        tsLog:None,
    }
}

let setLogText = (st,str) => {
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

@react.component
let make = () => {
    let modalRef = useModalRef()
    let (state, setState) = React.useState(makeInitialState)

    let actTsLogTextChanged = str => {
        setState(setLogText(_, str))
    }
    
    let actTsLogChanged = arrOpt => {
        setState(setLog(_, arrOpt))
    }

    let actCalculate = () => {
        switch state.tsLogText->parseTimeSheet {
            | Error(msg) => openInfoDialog(~modalRef, ~text=`Error: ${msg}`, ())
            | Ok(tsLog) => {
                if (tsLog->Js_array2.length == 0) {
                    openInfoDialog(~modalRef, ~text=`The timesheet is empty.`, ())
                } else {
                    actTsLogChanged(Some(tsLog))
                }
            }
        }
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
                {React.string("Time")}
            </th>
        </tr>
    }

    let rndLogRecord = (~logRec:tsLogRecord, ~id:int) => {
        <tr key={id->Belt_Int.toString} className="highlighted-on-hover">
            <td className="table-single-border timesheet-cell">
                {logRec.date->dateToString->React.string}
            </td>
            <td className="table-single-border timesheet-cell">
                {
                    if (logRec.isHoliday) {
                        React.string("holiday")
                    } else {
                        React.null
                    }
                }
            </td>
            <td className="table-single-border timesheet-cell">
                {logRec.durMinutes->minutesToDurStr->React.string}
            </td>
        </tr>
    }

    let rndContent = () => {
        switch state.tsLog {
            | None => {
                <Col>
                    <TextField
                        label="Timesheet data"
                        size=#small
                        style=ReactDOM.Style.make(~width="500px", ())
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
                            {tsLog->Js.Array2.mapi((logRec,i) => rndLogRecord(~logRec, ~id=i))->React.array}
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