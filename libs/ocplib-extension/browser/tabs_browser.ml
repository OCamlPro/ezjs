open Js_types
open Promise
include Tabs_browser_common

let captureTab ?id ?options f =
  jthen (tabs##captureTab (def_option id) (def_option options))
    (fun s -> f (to_string s))
let captureVisibleTab ?id ?options f =
  jthen (tabs##captureVisibleTab (def_option id) (def_option options))
    (fun s -> f (to_string s))
let create ?callback tab = jthen_opt (tabs##create tab) callback
let detectLanguage ?id f =
  jthen (tabs##detectLanguage (def_option id) (wrap_callback (fun s -> f (to_string s))))
    (fun s -> f (to_string s))
let discard ?callback id = jthen_opt (tabs##discard id) callback
let discard_list ?callback ids =
  jthen_opt (tabs##discard_arr (array_of_list ids)) callback
let duplicate ?callback id = jthen_opt (tabs##duplicate id) callback
let executeScript ?id ?callback details =
  jthen_opt (tabs##executeScript (def_option id) details) callback
let get id f = jthen (tabs##get id) f
let getCurrent f = jthen tabs##getCurrent f
let getZoom ?id f = jthen (tabs##getZoom (def_option id)) (fun x -> f (float_of_number x))
let getZoomSettings ?id f = jthen (tabs##getZoomSettings (def_option id)) f
let highlight ?callback info = jthen_opt (tabs##highlight info) callback
let move ?callback id props = jthen_opt (tabs##move id props) callback
let move_list ?callback ids props =
  jthen_opt (tabs##move_arr (array_of_list ids) props) callback
let query info f = jthen (tabs##query info) f
let reload ?id ?props ?callback () =
  jthen_opt (tabs##reload (def_option id) (def_option props)) callback
let remove ?callback id = jthen_opt (tabs##remove id) callback
let saveAsPDF settings f = jthen (tabs##saveAsPDF settings) (fun s -> f (to_string s))
let remove_list ?callback id = jthen_opt (tabs##remove_arr (array_of_list id)) callback
let sendMessage ?details ?callback id message =
  jthen_opt (tabs##sendMessage id message (def_option details)) callback
let setZoom ?id ?callback factor =
  jthen_opt (tabs##setZoom (def_option id) (number_of_float factor)) callback
let setZoomSettings ?id props f =
  jthen (tabs##setZoomSettings (def_option id) props) f
let update ?id ?callback props =
  jthen_opt (tabs##update (def_option id) props) callback
