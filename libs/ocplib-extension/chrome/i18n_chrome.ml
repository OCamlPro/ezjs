open Js_types
include I18n_chrome_common

let getAcceptLanguages f =
  i18n##getAcceptLanguages (wrap_callback (fun a -> f (array_to_list_str a)))
let detectLanguage text f = i18n##detectLanguage (string text) (wrap_callback f)
