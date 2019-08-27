module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html
module Firebug = Js_of_ocaml.Firebug
module Url = Js_of_ocaml.Url
module File = Js_of_ocaml.File
module Dom = Js_of_ocaml.Dom
module Regexp = Js_of_ocaml.Regexp
module Json = Js_of_ocaml.Json
module XmlHttpRequest = Js_of_ocaml.XmlHttpRequest
module Typed_array = Js_of_ocaml.Typed_array

module Html = Js_of_ocaml_tyxml.Tyxml_js.Html5
module Of_dom = Js_of_ocaml_tyxml.Tyxml_js.Of_dom
module To_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom
module Xml = Js_of_ocaml_tyxml.Tyxml_js.Xml

type 'a elt = 'a Html.elt
