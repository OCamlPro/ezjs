open Js_types
open Promise
include I18n_browser_common

let getAcceptLanguages f =
  jthen i18n##getAcceptLanguages (fun a -> f (array_to_list_str a))
let detectLanguage text = i18n##detectLanguage (string text)
