open Js_types
open Promise
open Browser_utils_lwt

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

let storage : storage t = variable "chrome.storage"
let local = storage##.local
let sync = storage##.sync
let managed = storage##.managed

let get ?key (st:storageArea t) =
  to_lwt_cb (fun cb -> st##get (opt string key) cb)
let get_arr ?keys (st:storageArea t) =
  let keys = opt array_of_list_str keys in
  to_lwt_cb (fun cb -> st##get_arr keys cb)
let get_o ?obj (st:storageArea t) =
  to_lwt_cb (fun cb -> st##get (option obj) cb)
let getBytesInUse ?key (st:storageArea t) =
  to_lwt_cb (fun cb -> st##getBytesInUse (opt string key) cb)
let getBytesInUse_list ?keys (st:storageArea t) =
  let keys = opt array_of_list_str keys in
  to_lwt_cb (fun cb -> st##getBytesInUse_arr keys cb)
let set ?callback (st:storageArea t) o =
  to_lwt_cb_opt callback (fun cb -> st##set o cb)
let remove ?callback (st:storageArea t) s =
  to_lwt_cb_opt callback (fun cb -> st##remove (string s) cb)
let clear ?callback (st:storageArea t) =
  to_lwt_cb_opt callback (fun cb -> st##clear cb)
