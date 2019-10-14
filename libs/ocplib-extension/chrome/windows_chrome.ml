open Js_types
include Windows_chrome_common

let get ?info id f = windows##get id (def_option info) (wrap_callback f)
let getCurrent ?info f = windows##getCurrent (def_option info) (wrap_callback f)
let getLastFocused ?info f = windows##getLastFocused (def_option info) (wrap_callback f)
let getAll ?info f =
  windows##getAll (def_option info) (wrap_callback (fun a -> f (array_to_list a)))
let create ?info ?callback () =
  windows##create (def_option info) (optdef_wrap callback)
let update ?callback id info =
  windows##update id info (optdef_wrap callback)
let remove ?callback id = windows##remove id (optdef_wrap callback)
