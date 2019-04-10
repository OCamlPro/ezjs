module Js = Js_of_ocaml.Js

class type input = object
  method action : Js.js_string Js.t Js.prop
end

class type result = object
  method _then : (Js.js_string Js.t -> unit) -> unit Js.meth
end

class type grecaptcha  = object
  method ready : (unit -> unit) -> unit Js.meth
  method execute : Js.js_string Js.t -> input Js.t -> result Js.t Js.meth
end


let recaptcha : grecaptcha Js.t = Js.Unsafe.variable "grecaptcha"

let check ?(action="login") site_key f =
  recaptcha##ready (fun () ->
      let a : input Js.t = Js.Unsafe.obj [||] in
      a##action <- Js.string action ;
      let res = recaptcha##execute (Js.string site_key, a) in
      res##_then(fun token -> f (Js.to_string token))
    )
