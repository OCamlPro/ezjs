open Js_types
open Promise

class type storageChange = object
  method oldValue : 'a prop
  method newValue : 'a prop
end

class type storageArea = object
  method get : js_string t opt -> 'a t promise t meth
  method get_arr : js_string t js_array t opt -> 'a t promise t meth
  method get_o : 'a t opt -> 'a t promise t meth
  method getBytesInUse : js_string t opt -> int promise t meth
  method getBytesInUse_arr : js_string t js_array t opt -> int promise t meth
  method set : 'b t -> unit promise t meth
  method remove : js_string t -> unit promise t meth
  method clear : unit promise t meth
end

class type storage = object
  method sync : storageArea t prop
  method local : storageArea t prop
  method managed : storageArea t prop
end

let storage : storage t = variable "browser.storage"
let local = storage##.local
let sync = storage##.sync
let managed = storage##.managed

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
