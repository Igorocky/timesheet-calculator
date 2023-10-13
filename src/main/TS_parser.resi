open TS_common

let durMinutesFromString: string => option<int>
let dateFromString: string => option<date>
let tsLogRecordFromString: string => result<TS_model.tsLogRecord, string>
let parseTimesheet: string => result<array<TS_model.tsLogRecord>,string>
