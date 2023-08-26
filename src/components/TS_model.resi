type date = {
    year: int, 
    month: int, 
    day: int
}

type tsLogRecord = {
    date: date,
    isHoliday:bool,
    durMinutes: int,
}

let dateToString: date => string
let minutesToDurStr: int => string