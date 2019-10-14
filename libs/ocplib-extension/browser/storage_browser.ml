open Js_types
open Promise
include Storage_browser_common

let get ?key (st:storageArea t) f = jthen (st##get (opt string key)) f
let get_arr ?keys (st:storageArea t) f =
  let keys = opt array_of_list_str keys in
  jthen (st##get_arr keys) f
let get_o ?obj (st:storageArea t) f = jthen (st##get_o (option obj)) f
let getBytesInUse ?key (st:storageArea t) f =
  jthen (st##getBytesInUse (opt string key)) f
let getBytesInUse_list ?keys (st:storageArea t) f =
  jthen (st##getBytesInUse_arr (opt array_of_list_str keys)) f
let set ?callback (st:storageArea t) o = jthen_opt (st##set o) callback
let remove ?callback (st:storageArea t) s = jthen_opt (st##remove (string s)) callback
let clear ?callback (st:storageArea t) = jthen_opt st##clear callback
