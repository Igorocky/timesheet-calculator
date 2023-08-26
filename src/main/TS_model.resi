type date = {
    year: int, 
    month: int, 
    day: int
}

type tsLogRecord = {
    date: date,
    durMinutes: int,
}

let dateIsWeekend: date => bool
let dateToString: date => string
let minutesToDurStr: int => string