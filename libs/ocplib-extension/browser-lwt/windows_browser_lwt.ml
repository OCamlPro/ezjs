open Ezjs_min
open Js
open Promise_lwt
include Windows_browser_common

let get ?info id = to_lwt (windows##get id (Optdef.option info))
let getCurrent ?info () = to_lwt (windows##getCurrent (Optdef.option info))
let getLastFocused ?info () = to_lwt (windows##getLastFocused (Optdef.option info))
let getAll ?info () =
  to_lwt_tr to_list (windows##getAll (Optdef.option info))
let create ?info ?callback () =
  to_lwt_opt callback (windows##create (Optdef.option info))
let update ?callback id info =
  to_lwt_opt callback (windows##update id info)
let remove ?callback id =
  to_lwt_opt callback (windows##remove id)
