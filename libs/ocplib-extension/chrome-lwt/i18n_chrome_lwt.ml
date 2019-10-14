open Js_types
open Promise_lwt
include I18n_chrome_common

let getAcceptLanguages () =
  to_lwt_cb_tr array_to_list_str i18n##getAcceptLanguages
let detectLanguage text =
  to_lwt_cb (fun cb -> i18n##detectLanguage (string text) cb)
