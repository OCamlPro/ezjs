open Js_types
open Promise_lwt
include Windows_browser_common

let get ?info id = to_lwt (windows##get id (def_option info))
let getCurrent ?info () = to_lwt (windows##getCurrent (def_option info))
let getLastFocused ?info () = to_lwt (windows##getLastFocused (def_option info))
let getAll ?info () =
  to_lwt_tr array_to_list (windows##getAll (def_option info))
let create ?info ?callback () =
  to_lwt_opt callback (windows##create (def_option info))
let update ?callback id info =
  to_lwt_opt callback (windows##update id info)
let remove ?callback id =
  to_lwt_opt callback (windows##remove id)
