open Ocp_js

let path_string () = Url.Current.path_string

let url () =
  match Url.url_of_string (Js.to_string Dom_html.window##location##href) with
  | None -> assert false
  | Some url -> url

let host () =
  match url () with
  | Url.Http hu | Url.Https hu -> hu.Url.hu_host
  | Url.File _fu -> "localhost"

let args () =
  match url () with
  | Url.Http hu | Url.Https hu -> hu.Url.hu_arguments
  | Url.File fu -> fu.Url.fu_arguments

let path () =
  match url () with
  | Url.Http hu | Url.Https hu -> hu.Url.hu_path
  | Url.File fu -> fu.Url.fu_path

let find_arg arg =
  try
    Some (List.assoc arg (args ()))
  with Not_found -> None

let lang () = find_arg "lang"

let proto () =
  match url () with
  | Url.Https _ -> "https://"
  | Url.File _ -> "file://"
  | Url.Http _ -> "http://"

let set_args args =
  let url = match url () with
    | Url.Http hu -> Url.Http { hu with Url.hu_arguments = args }
    | Url.Https hu -> Url.Https { hu with Url.hu_arguments = args }
    | Url.File fu -> Url.File { fu with Url.fu_arguments = args }
  in
  Dom_html.window##history##replaceState(
      Js.Opt.empty,
      Js.string "",
      Js.some (Js.string (Url.string_of_url url)))

let set_url url =
  Dom_html.window##location##href <-
    Js.string (Url.string_of_url url)
