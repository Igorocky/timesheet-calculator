open Expln_test
open TS_model
open TS_parser
open TS_common

// external cast: 'a => 'b = "%identity"

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
            ~regularWorkDurationMinutes=8*60,
            ~regularRatePerHour=7.0,
            ~overtimeRatePerHour=9.0,
            ~weekendRatePerHour=11.0,
        )

        //then
        assertEq( 
            tsCalc, 
            [
                {
                    date: { year: 2023, month: 9, day: 1 },
                    amount: 45.5,
                    amountSum: 45.5,
                    amountFormula: "45.50 = (6 hrs 30 min) * 7.00",
                    regularDurMinutesSum: 390,
                    regularAmountSum: 45.5,
                    overtimeDurMinutesSum: 0,
                    overtimeAmountSum: 0.,
                    weekendDurMinutesSum: 0,
                    weekendAmountSum: 0.
                },
                {
                    date: { year: 2023, month: 9, day: 2 },
                    amount: 34.833333333333336,
                    amountSum: 80.33333333333334,
                    amountFormula: "34.83 = (3 hrs 10 min) * 11.00",
                    regularDurMinutesSum: 390,
                    regularAmountSum: 45.5,
                    overtimeDurMinutesSum: 0,
                    overtimeAmountSum: 0.,
                    weekendDurMinutesSum: 190,
                    weekendAmountSum: 34.833333333333336
                },
                {
                    date: { year: 2023, month: 9, day: 3 },
                    amount: 0.,
                    amountSum: 80.33333333333334,
                    amountFormula: "",
                    regularDurMinutesSum: 390,
                    regularAmountSum: 45.5,
                    overtimeDurMinutesSum: 0,
                    overtimeAmountSum: 0.,
                    weekendDurMinutesSum: 190,
                    weekendAmountSum: 34.833333333333336
                },
                {
                    date: { year: 2023, month: 9, day: 4 },
                    amount: 56.,
                    amountSum: 136.33333333333334,
                    amountFormula: "56.00 = (8 hrs 0 min) * 7.00",
                    regularDurMinutesSum: 870,
                    regularAmountSum: 101.5,
                    overtimeDurMinutesSum: 0,
                    overtimeAmountSum: 0.,
                    weekendDurMinutesSum: 190,
                    weekendAmountSum: 34.833333333333336
                },
                {
                    date: { year: 2023, month: 9, day: 5 },
                    amount: 74.,
                    amountSum: 210.33333333333334,
                    amountFormula: "74.00 = (8 hrs 0 min) * 7.00 + (2 hrs 0 min) * 9.00",
                    regularDurMinutesSum: 1350,
                    regularAmountSum: 157.5,
                    overtimeDurMinutesSum: 120,
                    overtimeAmountSum: 18.,
                    weekendDurMinutesSum: 190,
                    weekendAmountSum: 34.833333333333336
                },
                {
                    date: { year: 2023, month: 9, day: 6 },
                    amount: 77.,
                    amountSum: 287.33333333333337,
                    amountFormula: "77.00 = (8 hrs 0 min) * 7.00 + (2 hrs 20 min) * 9.00",
                    regularDurMinutesSum: 1830,
                    regularAmountSum: 213.5,
                    overtimeDurMinutesSum: 260,
                    overtimeAmountSum: 39.,
                    weekendDurMinutesSum: 190,
                    weekendAmountSum: 34.833333333333336
                },
                {
                    date: { year: 2023, month: 9, day: 7 },
                    amount: 0.,
                    amountSum: 287.33333333333337,
                    amountFormula: "",
                    regularDurMinutesSum: 1830,
                    regularAmountSum: 213.5,
                    overtimeDurMinutesSum: 260,
                    overtimeAmountSum: 39.,
                    weekendDurMinutesSum: 190,
                    weekendAmountSum: 34.833333333333336
                },
                {
                    date: { year: 2023, month: 9, day: 8 },
                    amount: 0.,
                    amountSum: 287.33333333333337,
                    amountFormula: "",
                    regularDurMinutesSum: 1830,
                    regularAmountSum: 213.5,
                    overtimeDurMinutesSum: 260,
                    overtimeAmountSum: 39.,
                    weekendDurMinutesSum: 190,
                    weekendAmountSum: 34.833333333333336
                },
                {
                    date: { year: 2023, month: 9, day: 9 },
                    amount: 66.,
                    amountSum: 353.33333333333337,
                    amountFormula: "66.00 = (6 hrs 0 min) * 11.00",
                    regularDurMinutesSum: 1830,
                    regularAmountSum: 213.5,
                    overtimeDurMinutesSum: 260,
                    overtimeAmountSum: 39.,
                    weekendDurMinutesSum: 550,
                    weekendAmountSum: 100.83333333333334
                },
                {
                    date: { year: 2023, month: 9, day: 10 },
                    amount: 117.33333333333333,
                    amountSum: 470.6666666666667,
                    amountFormula: "117.33 = (10 hrs 40 min) * 11.00",
                    regularDurMinutesSum: 1830,
                    regularAmountSum: 213.5,
                    overtimeDurMinutesSum: 260,
                    overtimeAmountSum: 39.,
                    weekendDurMinutesSum: 1190,
                    weekendAmountSum: 218.16666666666669
                },
                {
                    date: { year: 2023, month: 9, day: 11 },
                    amount: 56.,
                    amountSum: 526.6666666666667,
                    amountFormula: "56.00 = (8 hrs 0 min) * 7.00",
                    regularDurMinutesSum: 2310,
                    regularAmountSum: 269.5,
                    overtimeDurMinutesSum: 260,
                    overtimeAmountSum: 39.,
                    weekendDurMinutesSum: 1190,
                    weekendAmountSum: 218.16666666666669
                },
                {
                    date: { year: 2023, month: 9, day: 12 },
                    amount: 0.,
                    amountSum: 526.6666666666667,
                    amountFormula: "",
                    regularDurMinutesSum: 2310,
                    regularAmountSum: 269.5,
                    overtimeDurMinutesSum: 260,
                    overtimeAmountSum: 39.,
                    weekendDurMinutesSum: 1190,
                    weekendAmountSum: 218.16666666666669
                }
            ]
        )
    })
})

