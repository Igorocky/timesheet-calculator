open Expln_test
open TS_model
open TS_parser

describe("durMinutesFromString", _ => {
    it("returnes results as expected", _ => {
        assertEq( durMinutesFromString("1:1"), Some(61))
        assertEq( durMinutesFromString("1:0"), Some(60))
        assertEq( durMinutesFromString("1"), Some(60))
        assertEq( durMinutesFromString("1.4"), None)
    })
})

describe("parseTimeSheet", _ => {
    it("parses with header", _ => {
        //given
        let tsWithHeader = `
            date	hours	minutes
            2023-08-25	3	15
            2023-08-26	5	0
        `

        //when
        let tsLog = parseTimesheet(tsWithHeader)

        //then
        assertEq(
            tsLog,
            Ok([
                {date:{year:2023,month:8,day:25},durMinutes:195},
                {date:{year:2023,month:8,day:26},durMinutes:300}
            ])
        )
    })

    it("parses without header", _ => {
        //given
        let tsWithHeader = `
            2023-08-25	3	15
            2023-08-26	0	50
        `

        //when
        let tsLog = parseTimesheet(tsWithHeader)

        //then
        assertEq(
            tsLog,
            Ok([
                {date:{year:2023,month:8,day:25},durMinutes:195},
                {date:{year:2023,month:8,day:26},durMinutes:50},
            ])
        )
    })
})
