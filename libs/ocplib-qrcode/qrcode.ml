(** Minimal binding of http://davidshimjs.github.io/qrcodejs/ *)

module Js = Js_of_ocaml.Js

class type qrcode  = object
  method makeCode : Js.js_string Js.t -> unit Js.meth
end

let make_code qrdiv text =
  let qrcode_ctsr = Js.Unsafe.global##._QRCode in
  let qrcode =
  (Js.Unsafe.new_obj qrcode_ctsr
     [| Js.Unsafe.inject qrdiv |] : qrcode Js.t) in
  qrcode##makeCode (Js.string text)
