open Js_of_ocaml

let unoptf f def x = match Js.Opt.to_option x with
  | None -> def
  | Some x -> f x

let get ?error msg url f =
  if msg <> "" then
    Firebug.console##log (Js.string @@ Printf.sprintf "[>%s GET %s]" msg url);
  let xhr = XmlHttpRequest.create () in
  xhr##(_open (Js.string "GET") (Js.string url) (Js._true)) ;
  xhr##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
        if xhr##.readyState = XmlHttpRequest.DONE then
          let status = xhr##.status in
          if msg <> "" then
            Firebug.console##log (Js.string @@ Printf.sprintf "[>%s RECV %d %s]" msg status url);
          if status = 200 then begin
              f (unoptf Js.to_string "" xhr##.responseText)
            end else
            match error with
            | None -> ()
            | Some f -> f status
      ) ;
  xhr##(send (Js.null))

let post ?(content_type="application/json") ?(content="{}") ?error msg url f =
  if msg <> "" then
    Firebug.console##log (Js.string @@ Printf.sprintf "[>%s POST %s]" msg url);
  let xhr = XmlHttpRequest.create () in
  xhr##(_open (Js.string "POST") (Js.string url) (Js._true)) ;
  xhr##(setRequestHeader
    (Js.string "Content-Type") (Js.string content_type)) ;
  xhr##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
        if xhr##.readyState = XmlHttpRequest.DONE then
          let status = xhr##.status in
          if msg <> "" then
            Firebug.console##log (Js.string @@ Printf.sprintf "[>%s RECV %d %s]" msg status url);
          if status = 200 then
            f (unoptf Js.to_string "" xhr##.responseText)
          else
            match error with
            | None -> ()
            | Some f -> f status
      ) ;
  xhr##(send (Js.some @@ Js.string content))
