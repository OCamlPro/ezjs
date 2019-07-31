open Js_min
open Js
open Browser_utils

class type storageChange = object
  method oldValue : 'a prop
  method newValue : 'a prop
end

class type storageArea = object
  method get : js_string t optdef -> 'a promise t meth
  method get_arr : js_string t js_array t optdef -> 'a js_array t promise t meth
  method getBytesInUse : js_string t optdef -> int promise t meth
  method getBytesInUse_arr : js_string t js_array t optdef -> int promise t meth
  method set : 'b t -> unit promise t meth
  method remove : js_string t -> unit promise t meth
  method clear : unit promise t meth
end

class type storage = object
  method sync : storageArea t prop
  method local : storageArea t prop
  method managed : storageArea t prop
end

let storage : storage t = Unsafe.variable "browser.storage"
let local = storage##local
let sync = storage##sync
let managed = storage##managed

let get ?key (st:storageArea t) f = jthen st##get(optdef string key) f
let get_list ?keys (st:storageArea t) f =
  let keys = optdef array_of_list_str keys in
  jthen st##get_arr(keys) (fun a -> f (array_to_list a))
let getBytesInUse ?key (st:storageArea t) f =
  jthen st##getBytesInUse(optdef string key) f
let getBytesInUse_list ?keys (st:storageArea t) f =
  jthen st##getBytesInUse_arr(optdef array_of_list_str keys) f
let set ?callback (st:storageArea t) o = jthen_opt st##set(o) callback
let remove ?callback (st:storageArea t) s = jthen_opt st##remove(string s) callback
let clear ?callback (st:storageArea t) = jthen_opt st##clear() callback
