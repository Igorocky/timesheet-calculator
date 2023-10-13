open Expln_test
open TS_model
open TS_parser
open TS_common

describe("dateIsWeekend", _ => {
    it("returns correct results", _ => {
        assertEq( dateFromString("2023-08-25")->Belt.Option.getExn->dateIsWeekend, false )
        assertEq( dateFromString("2023-08-26")->Belt.Option.getExn->dateIsWeekend, true )
        assertEq( dateFromString("2023-08-27")->Belt.Option.getExn->dateIsWeekend, true )
        assertEq( dateFromString("2023-08-28")->Belt.Option.getExn->dateIsWeekend, false )
    })
})

describe("tsCalculate", _ => {
    it("returns correct results", _ => {
        //given
        let tsData = `
            2023-09-01	6	30
            2023-09-02	3	10
            2023-09-03	0	0
            2023-09-04	8	0
            2023-09-05	10	0
            2023-09-06	10	20
            2023-09-07	0	0
            2023-09-08	0	0
            2023-09-09	6	0
            2023-09-10	10	40
            2023-09-11	8	0
            2023-09-12	0	0
        `
        let tsLog = parseTimesheet(tsData)->Belt.Result.getExn

        //when
        let tsCalc = tsCalculate(
            ~tsLog,
            ~regularWorkDurationHrs=8.0,
            ~regularRatePerHour=7.0,
            ~overtimeRatePerHour=9.0,
            ~weekendRatePerHour=11.0,
        )->Js.Array2.map(((_,tsCalc)) => tsCalc)

        //then
        assertEq( 
            tsCalc, 
            [
                {amount:45.5,formula:"45.50 = (6 hrs 30 min) * 7.00",sum:45.5, 
                    times: [Regular({durMinutes: 390, amount: 45.5, formula: "(6 hrs 30 min) * 7.00"})]},
                {amount:34.833333333333336,formula:"34.83 = (3 hrs 10 min) * 11.00",sum:80.33333333333334, 
                    times: [Weekend({durMinutes: 190, amount: 34.833333333333336, formula: "(3 hrs 10 min) * 11.00"})]},
                {amount:0.0,formula:"",sum:80.33333333333334, 
                    times: []},
                {amount:56.0,formula:"56.00 = (8 hrs 0 min) * 7.00",sum:136.33333333333334, 
                    times: [Regular({durMinutes: 480, amount: 56.0, formula: "(8 hrs 0 min) * 7.00"})]},
                {amount:74.0,formula:"74.00 = (8 hrs 0 min) * 7.00 + (2 hrs 0 min) * 9.00",sum:210.33333333333334, 
                    times: [
                        Regular({durMinutes: 480, amount: 56.0, formula: "(8 hrs 0 min) * 7.00"}), 
                        Overtime({durMinutes: 120, amount: 18.0, formula: "(2 hrs 0 min) * 9.00"})
                    ]
                },
                {amount:77.0,formula:"77.00 = (8 hrs 0 min) * 7.00 + (2 hrs 20 min) * 9.00",sum:287.33333333333337, 
                    times: [
                        Regular({durMinutes: 480, amount: 56.0, formula: "(8 hrs 0 min) * 7.00"}), 
                        Overtime({durMinutes: 140, amount: 21.0, formula: "(2 hrs 20 min) * 9.00"})
                    ]
                },
                {amount:0.0,formula:"",sum:287.33333333333337, 
                    times: []},
                {amount:0.0,formula:"",sum:287.33333333333337, 
                    times: []},
                {amount:66.0,formula:"66.00 = (6 hrs 0 min) * 11.00",sum:353.33333333333337, 
                    times: [Weekend({durMinutes: 360, amount: 66.0, formula: "(6 hrs 0 min) * 11.00"})]},
                {amount:117.33333333333333,formula:"117.33 = (10 hrs 40 min) * 11.00",sum:470.6666666666667, 
                    times: [Weekend({durMinutes: 640, amount: 117.33333333333333, formula: "(10 hrs 40 min) * 11.00"})]},
                {amount:56.0,formula:"56.00 = (8 hrs 0 min) * 7.00",sum:526.6666666666667, 
                    times: [Regular({durMinutes: 480, amount: 56.0, formula: "(8 hrs 0 min) * 7.00"})]},
                {amount:0.0,formula:"",sum:526.6666666666667, 
                    times: []},
            ]
        )
    })
})

describe("sumTimesByTypeForLog", _ => {
    it("returns correct results", _ => {
        //given
        let tsData = `
            2023-09-01	6	30
            2023-09-02	3	10
            2023-09-03	0	0
            2023-09-04	8	0
            2023-09-05	10	0
            2023-09-06	10	20
            2023-09-07	0	0
            2023-09-08	0	0
            2023-09-09	6	0
            2023-09-10	10	40
            2023-09-11	8	0
            2023-09-12	0	0
        `
        let tsLog = parseTimesheet(tsData)->Belt.Result.getExn
        let tsCalc = tsCalculate(
            ~tsLog,
            ~regularWorkDurationHrs=8.0,
            ~regularRatePerHour=7.0,
            ~overtimeRatePerHour=9.0,
            ~weekendRatePerHour=11.0,
        )

        //when
        let (
            (regularDur,regularMnt), 
            (overtimeDur,overtimeMnt), 
            (weekendDur,weekendMnt)
        ) = sumTimesByTypeForLog(tsCalc)

        //then
        assertEq( regularDur, 390+480+480+480+480 )
        assertEq( overtimeDur, 120+140 )
        assertEq( weekendDur, 190+360+640 )
        assertEq( regularMnt,  45.5 +. 56.0 +. 56.0 +. 56.0 +. 56.0 )
        assertEq( overtimeMnt, 18.0 +. 21.0 )
        assertEq( weekendMnt, 34.833333333333336 +. 66.0 +. 117.33333333333333 )
    })
})
