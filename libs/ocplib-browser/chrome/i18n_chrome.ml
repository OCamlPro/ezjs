open Js_types

class type language_percent = object
  method language : js_string t prop
  method percentage : int prop
end

class type language_detection = object
  method isReliable : bool t prop
  method languages : language_percent t js_array t prop
end

class type i18n = object
  method getAcceptLanguages : (js_string t js_array t -> unit) callback -> unit meth
  method getMessage : js_string t -> js_string t js_array t optdef -> js_string t meth
  method getUILanguage : js_string t meth
  method detectLanguage : js_string t -> (language_detection t -> unit) callback -> unit meth
end

let i18n = variable "chrome.i18n"

let getAcceptLanguages f =
  i18n##getAcceptLanguages (wrap_callback (fun a -> f (array_to_list_str a)))
let getMessage ?substitutions message =
  to_string @@ i18n##getMessage (string message) (optdef array_of_list_str substitutions)
let getUILanguage () = i18n##getUILanguage
let detectLanguage text f = i18n##detectLanguage (string text) (wrap_callback f)
