open Js_types
open Promise_lwt
include I18n_browser_common

let getAcceptLanguages () =
  to_lwt i18n##getAcceptLanguages >>= function
  | Error e -> return (Error e)
  | Ok a -> return (Ok (array_to_list_str a))
let detectLanguage text = to_lwt @@ i18n##detectLanguage (string text)
