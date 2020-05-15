open Js_min
open Promise_lwt
include Windows_chrome_common

let get ?info id =
  to_lwt_cb (fun cb -> windows##get id (Optdef.option info) cb)
let getCurrent ?info () =
  to_lwt_cb (fun cb -> windows##getCurrent (Optdef.option info) cb)
let getLastFocused ?info () =
  to_lwt_cb (fun cb -> windows##getLastFocused (Optdef.option info) cb)
let getAll ?info () =
  to_lwt_cb_tr to_list (fun cb -> windows##getAll (Optdef.option info) cb)
let create ?info ?callback () =
  to_lwt_cb_opt callback (fun cb -> windows##create (Optdef.option info) cb)
let update ?callback id info =
  to_lwt_cb_opt callback (fun cb -> windows##update id info cb)
let remove ?callback id =
  to_lwt_cb_opt callback (fun cb -> windows##remove id cb)
