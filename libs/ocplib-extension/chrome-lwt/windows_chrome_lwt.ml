open Js_types
open Promise_lwt
include Windows_chrome_common

let get ?info id =
  to_lwt_cb (fun cb -> windows##get id (def_option info) cb)
let getCurrent ?info () =
  to_lwt_cb (fun cb -> windows##getCurrent (def_option info) cb)
let getLastFocused ?info () =
  to_lwt_cb (fun cb -> windows##getLastFocused (def_option info) cb)
let getAll ?info () =
  to_lwt_cb_tr array_to_list (fun cb -> windows##getAll (def_option info) cb)
let create ?info ?callback () =
  to_lwt_cb_opt callback (fun cb -> windows##create (def_option info) cb)
let update ?callback id info =
  to_lwt_cb_opt callback (fun cb -> windows##update id info cb)
let remove ?callback id =
  to_lwt_cb_opt callback (fun cb -> windows##remove id cb)
