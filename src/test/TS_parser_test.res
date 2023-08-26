open Expln_test
open TS_model
open TS_parser

describe("parseTimeSheet", _ => {
    it("parses with header", _ => {
        //given
        let tsWithHeader = `
            date	is_holiday	hours	minutes
            2023-08-25	N	3	15
            2023-08-26	Y	5	0
        `

        //when
        let tsLog = parseTimeSheet(tsWithHeader)

        //then
        assertEq(
            tsLog,
            Ok([
                {date:{year:2023,month:8,day:25},isHoliday:false,durMinutes:195},
                {date:{year:2023,month:8,day:26},isHoliday:true,durMinutes:300}
            ])
        )
    })

    it("parses without header", _ => {
        //given
        let tsWithHeader = `
            2023-08-25	N	3	15
            2023-08-26	Y	0	50
        `

        //when
        let tsLog = parseTimeSheet(tsWithHeader)

        //then
        assertEq(
            tsLog,
            Ok([
                {date:{year:2023,month:8,day:25},isHoliday:false,durMinutes:195},
                {date:{year:2023,month:8,day:26},isHoliday:true,durMinutes:50}
            ])
        )
    })
})
