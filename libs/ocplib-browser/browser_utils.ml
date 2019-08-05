open Js_min
open Js

class type ['a] event = object
  method addListener : ('a -> unit) callback -> unit meth
  method removeListener : ('a -> unit) callback -> unit meth
  method hasListener : ('a -> unit) callback -> unit meth
end

class type ['a, 'b] event2 = object
  method addListener : ('a -> 'b -> unit) callback -> unit meth
  method removeListener : ('a -> 'b -> unit) callback -> unit meth
  method hasListener : ('a -> 'b -> unit) callback -> unit meth
end

class type ['a, 'b, 'c] event3 = object
  method addListener : ('a -> 'b -> 'c -> unit) callback -> unit meth
  method removeListener : ('a -> 'b -> 'c -> unit) callback -> unit meth
  method hasListener : ('a -> 'b -> 'c -> unit) callback -> unit meth
end

class type ['a] promise = object
  method _then : ('a -> unit) -> 'a promise t meth
  method catch : ('a -> unit) -> 'a promise t meth
end

class type errorBrowser = object
  method message : js_string t prop
  method nomFichier : js_string t prop
  method numeroLigne : int prop
end

type error_browser = {
  error_message : string;
  error_fichier : string;
  error_ligne : int
}

let of_error_browser {error_message; error_fichier; error_ligne} =
  let o : errorBrowser t = Unsafe.obj [||] in
  o##message <- string error_message;
  o##nomFichier <- string error_fichier;
  o##numeroLigne <- error_ligne;
  o

let to_error_browser (o:errorBrowser t) = {
  error_message = to_string o##message;
  error_fichier = to_string o##nomFichier;
  error_ligne = o##numeroLigne
}

let addListener (e:'a event t) f = e##addListener(wrap_callback f)
let addListener2 (e:('a, 'b) event2 t) f = e##addListener(wrap_callback f)
let addListener3 (e:('a, 'b, 'c) event3 t) f = e##addListener(wrap_callback f)
let removeListener (e:'a event t) f = e##removeListener(wrap_callback f)
let removeListener2 (e:('a, 'b) event2 t) f = e##removeListener(wrap_callback f)
let removeListener3 (e:('a, 'b, 'c) event3 t) f = e##removeListener(wrap_callback f)
let hasListener (e:'a event t) f = e##hasListener(wrap_callback f)
let hasListener2 (e:('a, 'b) event2 t) f = e##hasListener(wrap_callback f)
let hasListener3 (e:('a, 'b, 'c) event3 t) f = e##hasListener(wrap_callback f)

let optdef f = function
  | None -> undefined
  | Some x -> def (f x)
let optdef_wrap callback = optdef wrap_callback callback
let opt f = function
  | None -> null
  | Some x -> some (f x)
let opt_wrap callback = opt wrap_callback callback
let unopt_callback = function
  | None -> fun _ -> ()
  | Some callback -> callback
let array_of_list l = array (Array.of_list l)
let array_to_list a = Array.to_list (to_array a)
let array_of_list_str l = array (Array.of_list (List.map string l))
let array_to_list_str a = List.map to_string (Array.to_list (to_array a))

let jthen prom f = prom##_then(f)
let jthen_opt prom = function
  | None -> prom
  | Some f -> jthen prom f
