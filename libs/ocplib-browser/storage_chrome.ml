open Js_of_ocaml
open Js
open Browser_utils

class type storageChange = object
  method oldValue : 'a prop
  method newValue : 'a prop
end

class type storageArea = object
  method get : js_string t opt -> ('a t -> unit) callback -> unit meth
  method get_arr : js_string t js_array t opt -> ('a t -> unit) callback -> unit meth
  method get_o : 'a t opt -> ('a t -> unit) callback -> unit meth
  method getBytesInUse : js_string t opt -> (int -> unit) callback -> unit meth
  method getBytesInUse_arr : js_string t js_array t opt -> (int -> unit) callback -> unit meth
  method set : 'b t -> (unit -> unit) callback optdef -> unit meth
  method remove : js_string t -> (unit -> unit) callback optdef -> unit meth
  method clear : (unit -> unit) callback optdef -> unit meth
end

class type storage = object
  method sync : storageArea t prop
  method local : storageArea t prop
  method managed : storageArea t prop
end

let storage : storage t = Unsafe.variable "chrome.storage"
let local = storage##.local
let sync = storage##.sync
let managed = storage##.managed

let get ?key (st:storageArea t) f = st##get (opt string key) (wrap_callback f)
let get_arr ?keys (st:storageArea t) f =
  let keys = opt array_of_list_str keys in
  st##get_arr keys (wrap_callback f)
let get_o ?obj (st:storageArea t) f = st##get (Opt.option obj) (wrap_callback f)
let getBytesInUse ?key (st:storageArea t) f =
  st##getBytesInUse (opt string key) (wrap_callback f)
let getBytesInUse_list ?keys (st:storageArea t) f =
  let keys = opt array_of_list_str keys in
  st##getBytesInUse_arr keys (wrap_callback f)
let set ?callback (st:storageArea t) o = st##set o (optdef_wrap callback)
let remove ?callback (st:storageArea t) s = st##remove (string s) (optdef_wrap callback)
let clear ?callback (st:storageArea t) = st##clear (optdef_wrap callback)