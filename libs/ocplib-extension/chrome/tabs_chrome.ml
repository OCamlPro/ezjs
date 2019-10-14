open Js_types
include Tabs_chrome_common

let captureVisibleTab ?id ?options f =
  tabs##captureVisibleTab (def_option id) (def_option options)
    (wrap_callback (fun s -> f (to_string s)))
let create ?callback tab = tabs##create tab (optdef_wrap callback)
let detectLanguage ?id f =
  tabs##detectLanguage (def_option id) (wrap_callback (fun s -> f (to_string s)))
let discard ?id ?callback () = tabs##discard (def_option id) (optdef_wrap callback)
let duplicate ?callback id = tabs##duplicate id (optdef_wrap callback)
let executeScript ?id ?callback details =
  tabs##executeScript (def_option id) details (optdef_wrap callback)
let get id f = tabs##get id (wrap_callback f)
let getCurrent f = tabs##getCurrent (wrap_callback f)
let getZoom ?id f = tabs##getZoom (def_option id) (wrap_callback (fun x -> f (float_of_number x)))
let getZoomSettings ?id f = tabs##getZoomSettings (def_option id) (wrap_callback f)
let goBack ?id ?callback () = tabs##goBack (def_option id) (optdef_wrap callback)
let goForward ?id ?callback () = tabs##goForward (def_option id) (optdef_wrap callback)
let highlight ?callback info = tabs##highlight info (optdef_wrap callback)
let move ?callback id props = tabs##move id props (optdef_wrap callback)
let move_list ?callback ids props = tabs##move_arr (array_of_list ids) props (optdef_wrap callback)
let query info f = tabs##query info (wrap_callback f)
let reload ?id ?props ?callback () =
  tabs##reload (def_option id) (def_option props) (optdef_wrap callback)
let remove ?callback id = tabs##remove id (optdef_wrap callback)
let remove_list ?callback id = tabs##remove_arr (array_of_list id) (optdef_wrap callback)
let sendMessage ?details ?callback id message =
  tabs##sendMessage id message (def_option details) (optdef_wrap callback)
let setZoom ?id ?callback factor =
  tabs##setZoom (def_option id) (number_of_float factor) (optdef_wrap callback)
let setZoomSettings ?id props f =
  tabs##setZoomSettings (def_option id) props (wrap_callback f)
let update ?id ?callback props =
  tabs##update (def_option id) props (optdef_wrap callback)
