open Ezjs_min.Js
open Ezjs_min.Promise
include I18n_browser_common

let getAcceptLanguages f =
  jthen i18n##getAcceptLanguages (fun a -> f (of_listf string a))
let detectLanguage text = i18n##detectLanguage (string text)
