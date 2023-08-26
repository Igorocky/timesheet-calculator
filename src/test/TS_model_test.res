open Expln_test
open TS_model
open TS_parser

describe("dateIsWeekend", _ => {
    it("returns correct results", _ => {
        assertEq( dateFromString("2023-08-25")->Belt.Option.getExn->dateIsWeekend, false )
        assertEq( dateFromString("2023-08-26")->Belt.Option.getExn->dateIsWeekend, true )
        assertEq( dateFromString("2023-08-27")->Belt.Option.getExn->dateIsWeekend, true )
        assertEq( dateFromString("2023-08-28")->Belt.Option.getExn->dateIsWeekend, false )
    })
})
