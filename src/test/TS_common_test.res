open Expln_test
open TS_common
open TS_parser

describe("floatParse", _ => {
    it("parses strings as expected", _ => {
        assertEqMsg( floatParse("0"), Some(0.), "0")
        assertEqMsg( floatParse("0.1"), Some(0.1), "0.1")
        assertEqMsg( floatParse("123.456"), Some(123.456), "123.456")

        assertEqMsg( floatParse("0."), None, "0.")
        assertEqMsg( floatParse("0,1"), None, "0,1")
        assertEqMsg( floatParse(".1"), None, ".1")
        assertEqMsg( floatParse(",1"), None, ",1")
        assertEqMsg( floatParse("123,456"), None, "123,456")

        assertEqMsg( floatParse("a"), None, "a")
        assertEqMsg( floatParse("0a"), None, "0a")
        assertEqMsg( floatParse("a0"), None, "a0")
    })
})

describe("dateIsWeekend", _ => {
    it("returns correct results", _ => {
        assertEq( dateFromString("2023-08-25")->Belt.Option.getExn->dateIsWeekend, false )
        assertEq( dateFromString("2023-08-26")->Belt.Option.getExn->dateIsWeekend, true )
        assertEq( dateFromString("2023-08-27")->Belt.Option.getExn->dateIsWeekend, true )
        assertEq( dateFromString("2023-08-28")->Belt.Option.getExn->dateIsWeekend, false )
    })
})