open Ocp_js

let on_load_initializers = ref []
let on_load_done = ref false

let on_load _ =
  let rec iter () =
    match !on_load_initializers with
    | [] -> ()
    | f :: tail ->
       (try f () with exn ->
          Js_utils.log "Warning: on_load raised exception %s\n%!"
                       (Printexc.to_string exn);
       );
       on_load_initializers := tail;
       iter ()
  in
  iter ();
  on_load_done := true;
  Js._true

let _ =
  Dom_html.window##onload <- Dom_html.handler on_load

let add f =
  if !on_load_done then f ()
  else
    on_load_initializers := !on_load_initializers @ [f]
