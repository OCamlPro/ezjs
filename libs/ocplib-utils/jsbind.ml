open Ocp_js

type string_field = Js.js_string Js.t Js.prop
type int_field = int Js.prop
type float_field = float Js.prop
type bool_field = bool Js.prop
type 'a array_field = 'a Js.js_array Js.t Js.prop

let eval_js txt =
  let _unit = Js.Unsafe.eval_string txt in
  ()

let link_js scriptUrl f =
  Xhr.get "jsreq" scriptUrl
          (fun txt ->
            eval_js txt;
            f ()
          )

let rec resolve_deps jsdeps baseurl f =
  match !jsdeps with
  | [] -> f ()
  | dep1 :: _tail ->
     link_js (baseurl ^ dep1) (fun () ->
             match !jsdeps with
             | [] -> f ()
             | dep2 :: tail ->
                if dep1 = dep2 then begin
                    jsdeps := tail;
                    resolve_deps jsdeps baseurl f
                  end
                else
                  f ())
