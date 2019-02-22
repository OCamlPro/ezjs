open Ocp_js

let init () = ()

let make () = Html.div ~a:[ Html.a_id "content" ] []

let update new_content =
  Js_utils.Manip.replaceChildren (Js_utils.find_component "content") new_content
