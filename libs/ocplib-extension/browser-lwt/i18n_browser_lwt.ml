open Ezjs_min
open Js
open Promise_lwt
include I18n_browser_common

let getAcceptLanguages () =
  to_lwt i18n##getAcceptLanguages >>= function
  | Error e -> return (Error e)
  | Ok a -> return (Ok (to_listf to_string a))
let detectLanguage text = to_lwt (i18n##detectLanguage (string text))
