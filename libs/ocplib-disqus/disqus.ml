open Ocp_js

let first = ref true

class type page = object
  method url: Js.js_string Js.t Js.prop
  method identifier: Js.js_string Js.t Js.prop
  method title: Js.js_string Js.t Js.prop
end

class type disqus_reset = object
  method reload: bool Js.t Js.prop
  method config: (unit -> unit) Js.prop
end

class type disqus = object
  method reset: disqus_reset Js.t -> unit Js.meth
  method page: page Js.t Js.prop
  method language: Js.js_string Js.t Js.prop
end

let build_config ?(lang="en") ?title url identifier () =
  let title = match title with
    | None -> Printf.sprintf "Thread for %s" identifier
    | Some title -> title in
  let disqus : disqus Js.t = Js.Unsafe.variable "DISQUS" in
  let page_config : page Js.t = Js.Unsafe.obj [||] in
  page_config##identifier <- Js.string identifier;
  page_config##url <- Js.string url;
  page_config##title <- Js.string title;
  disqus##language <- Js.string lang;
  disqus##page <- page_config

let init ?lang ?title url identifier name =
  let config = build_config ?lang ?title url identifier in
  Js.Unsafe.global##disqus_config <- config;
  let doc = Dom_html.document in
  let s = doc##createElement(Js.string "script") in
  s##setAttribute(Js.string "src", Js.string ("https://" ^ name ^ ".disqus.com/embed.js"));
  s##setAttribute(Js.string "data-timestamp", (jsnew Js.date_now ())##toLocaleString());
  s##setAttribute(Js.string "async", Js.string "true");
  Js_utils.Manip.appendChild
    (Tyxml_js.Of_dom.of_body doc##body)
    (Tyxml_js.Of_dom.of_element s)

let reset ?lang ?title url identifier =
  let disqus : disqus Js.t = Js.Unsafe.variable "DISQUS" in
  let disqus_obj : disqus_reset Js.t = Js.Unsafe.obj [||] in
  disqus_obj##reload <- Js._true;
  disqus_obj##config <- build_config ?lang ?title url identifier;
  disqus##reset(disqus_obj)


let load ?lang ?title url identifier name =
  if !first then (
    first := false;
    init ?lang ?title url identifier name)
  else reset ?lang ?title url identifier
