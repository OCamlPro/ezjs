open Fetch
open Js_of_ocaml

class type test = object
  method hash : Js.js_string Js.t Js.prop
end

let () = Lwt.async @@ fun () ->
  fetch "https://api.dunscan.io/v4/head" to_js >|=? fun (r : test Js.t response) ->
  Firebug.console##log r.body##.hash
