open TS_model

let dateFromString: string => option<date>
let tsLogRecordFromString: string => result<tsLogRecord, string>
let parseTimeSheet: string => result<array<tsLogRecord>,string>
