open Js_of_ocaml

type 'a t = 'a Js.t
type 'a prop = 'a Js.prop
type 'a meth = 'a Js.meth
type 'a constr = 'a Js.constr
type js_string = Js.js_string
type 'a js_array = 'a Js.js_array
type 'a opt = 'a Js.opt
type 'a optdef = 'a Js.optdef
type 'a callback = 'a Js.callback
type number = Js.number
type window = Dom_html.window
type error = Js.error

let string = Js.string
let bool = Js.bool
let array = Js.array
let to_string = Js.to_string
let to_bool = Js.to_bool
let to_array = Js.to_array
let some = Js.some
let def = Js.def
let null = Js.null
let undefined = Js.undefined
let wrap_callback = Js.wrap_callback
let option = Js.Opt.option
let def_option = Js.Optdef.option
let to_option = Js.Opt.to_option
let to_def_option = Js.Optdef.to_option
let opt f = function
  | None -> null
  | Some x -> some (f x)
let optdef f = function
  | None -> undefined
  | Some x -> def (f x)

let obj = Js.Unsafe.obj
let variable = Js.Unsafe.variable
let coerce = Js.Unsafe.coerce

let array_of_list l = array (Array.of_list l)
let array_to_list a = Array.to_list (to_array a)
let array_of_list_str l = array (Array.of_list (List.map string l))
let array_to_list_str a = List.map to_string (Array.to_list (to_array a))

let number_of_float = Js.number_of_float
let float_of_number = Js.float_of_number
