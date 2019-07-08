open Ocp_js
open Js


(* Date constructor *)
let date_now () = jsnew date_now ()
let date_fromTimeValue t = jsnew date_fromTimeValue(t)
let date_month y m = jsnew date_month(y, m)
let date_day y m d = jsnew date_day(y, m, d)
let date_hour y m d h = jsnew date_hour(y, m, d, h)
let date_min y m d h mi = jsnew date_min(y, m, d, h, mi)
let date_sec y m d h mi s = jsnew date_sec(y, m, d, h, mi, s)
let date_ms y m d h mi s ms = jsnew date_ms(y, m, d, h, mi, s, ms)

(* Date methods *)
let toString d = to_string d##toString()
let toDateString d = to_string d##toDateString()
let toTimeString d = to_string d##toTimeString()
let toLocaleString d = to_string d##toLocaleString()
let toLocaleDateString d = to_string d##toLocaleDateString()
let toLocaleTimeString d = to_string d##toLocaleTimeString()

(* getters *)
let getTime d = d##getTime()
let getFullYear ?(utc=false) d =
  if not utc then d##getFullYear() else d##getUTCFullYear()
let getMonth ?(utc=false) d =
  if not utc then d = d##getMonth() else d##getUTCMonth()
let getDate ?(utc=false) d =
  if not utc then d##getDate() else d##getUTCDate()
let getDay ?(utc=false) d =
  if not utc then d##getDay() else d##getUTCDay()
let getHours ?(utc=false) d =
  if not utc then d##getHours() else d##getUTCHours()
let getMinutes ?(utc=false) d =
  if not utc then d##getMinutes() else d##getUTCMinutes()
let getSeconds ?(utc=false) d =
  if not utc then d##getSeconds() else d##getUTCSeconds()
let getMilliseconds ?(utc=false) d =
  if not utc then d##getMilliseconds() else d##getUTCMilliseconds()
let getTimesoneOffset d = d##getTimezoneOffset()

(* setters *)
let setTime d = d##setTime()
let setFullYear ?(utc=false) d =
  if not utc then d##setFullYear() else d##setUTCFullYear()
let setMonth ?(utc=false) d =
  if not utc then d = d##setMonth() else d##setUTCMonth()
let setDate ?(utc=false) d =
  if not utc then d##setDate() else d##setUTCDate()
let setDay ?(utc=false) d =
  if not utc then d##setDay() else d##setUTCDay()
let setHours ?(utc=false) d =
  if not utc then d##setHours() else d##setUTCHours()
let setMinutes ?(utc=false) d =
  if not utc then d##setMinutes() else d##setUTCMinutes()
let setSeconds ?(utc=false) d =
  if not utc then d##setSeconds() else d##setUTCSeconds()
let setMilliseconds ?(utc=false) d =
  if not utc then d##setMilliseconds() else d##setUTCMilliseconds()


(* utils *)
let valueOf d = d##valueOf()
let toUTCString d = to_string d##toUTCString()
let toISOString d = to_string d##toISOString()

let parse str = date##parse(Js.string str)

let now_value () = valueOf @@ date_now ()

let now_tsp () = toLocaleString @@ date_now ()
let value_tsp t = toLocaleString @@ date_fromTimeValue t
