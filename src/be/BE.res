open Expln_utils_jsonParse
open Expln_BE_utils

type method1Req = { msg: string }
type method1Resp = { len: int }
let method1:beFunc<method1Req, method1Resp> = createBeFunc("/method1", d => { len: d->int("len", ()) } )