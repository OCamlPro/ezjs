open Js_types
open Promise
include Windows_browser_common

let get ?info id f = jthen (windows##get id (def_option info)) f
let getCurrent ?info f = jthen (windows##getCurrent (def_option info)) f
let getLastFocused ?info f = jthen (windows##getLastFocused (def_option info)) f
let getAll ?info f =
  jthen (windows##getAll (def_option info)) (fun a -> f (array_to_list a))
let create ?info ?callback () =
  jthen_opt (windows##create (def_option info)) callback
let update ?callback id info =
  jthen_opt (windows##update id info) callback
let remove ?callback id = jthen_opt (windows##remove id) callback
