open Js_of_ocaml

let get_session () = Js.Optdef.to_option (Dom_html.window##.sessionStorage)

let get_value key =
  let key' = Js.string key in
  match get_session () with
  | None ->
    Firebug.console##log (Js.string "Session not found while getting value");
    None
  | Some session -> begin
      match Js.Opt.to_option (session##getItem key') with
      | None -> None
      | Some s ->
        let result = Js.to_string s in
        Some result
    end

let set_value key value =
  match get_session () with
  | None -> Firebug.console##log (Js.string "Session not found while setting value");
  | Some session ->
    let key   = Js.string key   in
    let value = Js.string value in
    session##setItem key value

let remove_value key =
  let key' = Js.string key in
  match get_session () with
  | None ->
    Firebug.console##log (Js.string "Session not found while removing value")
  | Some session -> begin
      match Js.Opt.to_option (session##getItem key') with
      | None -> ()
      | Some _ -> session##removeItem key'
    end
