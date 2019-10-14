open Js_types
open Promise_lwt
include Tabs_chrome_common

let captureVisibleTab ?id ?options () =
  to_lwt_cb_tr to_string (fun cb ->
      tabs##captureVisibleTab
        (def_option id) (def_option options) cb)
let create ?callback tab =
  to_lwt_cb_opt callback (fun cb -> tabs##create tab cb)
let detectLanguage ?id () =
  to_lwt_cb_tr to_string (fun cb -> tabs##detectLanguage (def_option id)  cb)
let discard ?id ?callback () =
  to_lwt_cb_opt callback (fun cb -> tabs##discard (def_option id) cb)
let duplicate ?callback id =
  to_lwt_cb_opt callback (fun cb -> tabs##duplicate id cb)
let executeScript ?id ?callback details =
  to_lwt_cb_opt callback (fun cb -> tabs##executeScript (def_option id) details cb)
let get id = to_lwt_cb (fun cb -> tabs##get id cb)
let getCurrent () = to_lwt_cb (fun cb -> tabs##getCurrent cb)
let getZoom ?id () =
  to_lwt_cb_tr float_of_number (fun cb -> tabs##getZoom (def_option id) cb)
let getZoomSettings ?id () =
  to_lwt_cb (fun cb -> tabs##getZoomSettings (def_option id) cb)
let goBack ?id ?callback () =
  to_lwt_cb_opt callback (fun cb -> tabs##goBack (def_option id) cb)
let goForward ?id ?callback () =
  to_lwt_cb_opt callback (fun cb -> tabs##goForward (def_option id) cb)
let highlight ?callback info =
  to_lwt_cb_opt callback (fun cb -> tabs##highlight info cb)
let insertCSS ?id () = tabs##insertCSS(def_option id)
let move ?callback id props =
  to_lwt_cb_opt callback (fun cb -> tabs##move id props cb)
let move_list ?callback ids props =
  to_lwt_cb_opt callback (fun cb -> tabs##move_arr (array_of_list ids) props cb)
let query info =
  to_lwt_cb (fun cb -> tabs##query info cb)
let reload ?id ?props ?callback () =
  to_lwt_cb_opt callback (fun cb -> tabs##reload (def_option id) (def_option props) cb)
let remove ?callback id =
  to_lwt_cb_opt callback (fun cb -> tabs##remove id cb)
let remove_list ?callback id =
  to_lwt_cb_opt callback (fun cb -> tabs##remove_arr (array_of_list id) cb)
let sendMessage ?details ?callback id message =
  to_lwt_cb_opt callback (fun cb ->
      tabs##sendMessage id message (def_option details) cb)
let setZoom ?id ?callback factor =
  to_lwt_cb_opt callback (fun cb ->
      tabs##setZoom (def_option id) (number_of_float factor) cb)
let setZoomSettings ?id props =
  to_lwt_cb (fun cb -> tabs##setZoomSettings (def_option id) props cb)
let update ?id ?callback props =
  to_lwt_cb_opt callback (fun cb -> tabs##update (def_option id) props cb)
