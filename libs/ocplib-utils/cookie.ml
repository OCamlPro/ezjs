open Ocp_js

type key = string
type value = string

let all () =
  let reg1 = Regexp.regexp "; " in
  let list = Regexp.split reg1 (Js.to_string Dom_html.document##.cookie) in
  let reg2 = Regexp.regexp "=" in
  List.map (fun s ->
    match Regexp.split reg2 s with
        x :: y -> (x, String.concat "=" y)
      | [] -> ("", "")
  ) list

let set key value =
  let today = new%js Js.date_now in
  let expire_date = new%js Js.date_ms
      (today##getFullYear + 1) (today##getMonth) (today##getDay)
       (today##getHours) (today##getMinutes) (today##getSeconds)
       (today##getMilliseconds) in
  let expire_time = Js.to_string expire_date##toUTCString in
  Dom_html.document##.cookie :=
    Js.string (Printf.sprintf "%s=%s;expires=%s" key value expire_time)

let set_with_timeout key value date =
  let expire_time = Js.to_string date##toUTCString in
  Dom_html.document##.cookie :=
    Js.string (Printf.sprintf "%s=%s;expires=%s" key value expire_time)


let clear key =
  Dom_html.document##.cookie :=
    Js.string (Printf.sprintf "%s=;expires=%s" key
                              "Thu, 01 Jan 1970 00:00:00 UTC")
