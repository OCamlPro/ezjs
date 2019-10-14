open Js_types
include Storage_chrome_common

let get ?key (st:storageArea t) f = st##get (opt string key) (wrap_callback f)
let get_arr ?keys (st:storageArea t) f =
  let keys = opt array_of_list_str keys in
  st##get_arr keys (wrap_callback f)
let get_o ?obj (st:storageArea t) f = st##get (option obj) (wrap_callback f)
let getBytesInUse ?key (st:storageArea t) f =
  st##getBytesInUse (opt string key) (wrap_callback f)
let getBytesInUse_list ?keys (st:storageArea t) f =
  let keys = opt array_of_list_str keys in
  st##getBytesInUse_arr keys (wrap_callback f)
let set ?callback (st:storageArea t) o = st##set o (optdef_wrap callback)
let remove ?callback (st:storageArea t) s = st##remove (string s) (optdef_wrap callback)
let clear ?callback (st:storageArea t) = st##clear (optdef_wrap callback)
