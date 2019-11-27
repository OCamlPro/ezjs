open Js_types
open Promise_lwt
include Storage_browser_common

let get ?key (st:storageArea t) =
  to_lwt @@ st##get (opt string key)
let get_arr ?keys (st:storageArea t) =
  let keys = opt array_of_list_str keys in
  to_lwt @@ st##get_arr keys
let get_o ?obj (st:storageArea t) =
  to_lwt @@ st##get_o (option obj)
let getBytesInUse ?key (st:storageArea t) =
  to_lwt @@ st##getBytesInUse (opt string key)
let getBytesInUse_list ?keys (st:storageArea t) =
  to_lwt @@ st##getBytesInUse_arr (opt array_of_list_str keys)
let set ?callback (st:storageArea t) o =
  to_lwt_opt callback (st##set o)
let remove ?callback (st:storageArea t) s =
  to_lwt_opt callback @@ st##remove (string s)
let clear ?callback (st:storageArea t) =
  to_lwt_opt callback st##clear
