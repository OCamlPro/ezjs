open Ezjs_min.Js
include I18n_chrome_common

let getAcceptLanguages f =
  i18n##getAcceptLanguages (wrap_callback (fun a -> f (of_listf string a)))
let detectLanguage text f = i18n##detectLanguage (string text) (wrap_callback f)
