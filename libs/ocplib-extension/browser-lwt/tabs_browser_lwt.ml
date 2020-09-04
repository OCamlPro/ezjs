open Ezjs_min
open Js
open Promise_lwt
open Browser_utils
open Runtime_utils
open Tabs_utils

class type tabs = object
  method _TAB_ID_NONE : js_string t prop
  method captureTab : int optdef -> imageDetails t optdef -> js_string t promise t meth
  method captureVisibleTab : int optdef -> imageDetails t optdef -> js_string t promise t meth
  method connect : int -> 'a t optdef -> port meth
  method create : createProperties t -> tab t promise t meth
  method detectLanguage : int optdef -> js_string t promise t meth
  method discard : int -> unit promise t meth
  method discard_arr : int js_array t -> unit promise t meth
  method duplicate : int -> tab t promise t meth
  method executeScript : int optdef -> details t -> 'a t js_array t promise t meth
  method get : int -> tab t promise t meth
  method getAllInWindow : int optdef -> tab t js_array t promise t meth
  method getCurrent : tab t promise t meth
  method getZoom : int optdef -> number t promise t meth
  method getZoomSettings : int optdef -> zoomSettings t promise t meth
  method hide : int -> js_string t js_array t promise t meth
  method hide_arr : int js_array t -> js_string t js_array t promise t meth
  method highlight : highlightInfo t -> window t promise t meth
  method insertCSS : int optdef -> details t meth
  method move : int -> moveProperties -> tab t promise t meth
  method move_arr : int js_array t -> moveProperties -> tab t js_array t promise t meth
  method moveInSuccession : int js_array t -> int optdef -> moveInSuccessionOptions t optdef -> unit meth
  method print : unit meth
  method printPreview : unit promise t meth
  method query : queryInfo t -> tab t js_array t promise t meth
  method reload : int optdef -> reloadProperties t optdef -> unit promise t meth
  method remove : int -> unit promise t meth
  method remove_arr : int js_array t -> unit promise t meth
  method removeCSS : int optdef -> details t -> unit promise t meth
  method saveAsPDF : pageSettings t -> js_string t promise t meth
  method sendMessage : int -> 'a t -> details t optdef -> 'b t promise t meth
  method setZoom : int optdef -> number t -> unit promise t meth
  method setZoomSettings : int optdef -> zoomSettings t -> unit promise t meth
  method show : int -> unit promise t meth
  method show_arr : int js_array t -> unit promise t meth
  method toggleReaderMode : int optdef -> unit promise t meth
  method update : int optdef -> tab t -> tab t promise t meth
  method onActivated : 'a t event t prop
  method onAttached : (int, 'a t) event2 t prop
  method onCreated : tab t event t prop
  method onDetached : (int, 'a t) event2 t prop
  method onHighlighted : 'a t event t prop
  method onMoved : (int, 'a t) event2 t prop
  method onRemoved : (int, 'a t) event2 t prop
  method onReplaced : (int, int) event2 t prop
  method onUpdated : (int, 'a t, tab t) event3 t prop
  method onZoomChanged : 'a t event t prop
end
let tabs : tabs t = Unsafe.variable "browser.tabs"
let tab_id_none () = tabs##._TAB_ID_NONE

let captureTab ?id ?options () =
  to_lwt_tr to_string (tabs##captureTab (Optdef.option id) (Optdef.option options))
let captureVisibleTab ?id ?options () =
  to_lwt_tr to_string (tabs##captureVisibleTab (Optdef.option id) (Optdef.option options))
let connect ?info id = tabs##connect id (Optdef.option info)
let create ?callback tab = to_lwt_opt callback (tabs##create tab)
let detectLanguage ?id () =
  to_lwt_tr to_string (tabs##detectLanguage (Optdef.option id))
let discard ?callback id = to_lwt_opt callback (tabs##discard id)
let discard_list ?callback ids =
  to_lwt_opt callback (tabs##discard_arr (of_list ids))
let duplicate ?callback id = to_lwt_opt callback (tabs##duplicate id)
let executeScript ?id ?callback details =
  to_lwt_opt callback (tabs##executeScript (Optdef.option id) details)
let get id = to_lwt (tabs##get id)
let getCurrent () = to_lwt tabs##getCurrent
let getZoom ?id () = to_lwt_tr float_of_number (tabs##getZoom (Optdef.option id))
let getZoomSettings ?id () = to_lwt (tabs##getZoomSettings (Optdef.option id))
let hide id = tabs##hide id
let hide_list ids = tabs##hide_arr (of_list ids)
let highlight ?callback info = to_lwt_opt callback (tabs##highlight info)
let insertCSS ?id () = tabs##insertCSS (Optdef.option id)
let move ?callback id props = to_lwt_opt callback (tabs##move id props)
let move_list ?callback ids props =
  to_lwt_opt callback (tabs##move_arr (of_list ids) props)
let moveInSuccession ?id ?options ids =
  tabs##moveInSuccession (of_list ids) (Optdef.option id) (Optdef.option options)
let print () = tabs##print
let printPreview () = tabs##printPreview
let query info = to_lwt (tabs##query info)
let reload ?id ?props ?callback () =
  to_lwt_opt callback (tabs##reload (Optdef.option id) (Optdef.option props))
let remove ?callback id = to_lwt_opt callback (tabs##remove id)
let removeCSS ?id details = tabs##removeCSS (Optdef.option id) details
let saveAsPDF settings = to_lwt_tr to_string (tabs##saveAsPDF settings)
let remove_list ?callback id = to_lwt_opt callback (tabs##remove_arr (of_list id))
let sendMessage ?details ?callback id message =
  to_lwt_opt callback (tabs##sendMessage id message (Optdef.option details))
let setZoom ?id ?callback factor =
  to_lwt_opt callback (tabs##setZoom (Optdef.option id) (number_of_float factor))
let setZoomSettings ?id props =
  to_lwt (tabs##setZoomSettings (Optdef.option id) props)
let show id = tabs##show id
let show_list ids = tabs##show_arr (of_list ids)
let toggleReaderMode ?id () = tabs##toggleReaderMode (Optdef.option id)
let update ?id ?callback props =
  to_lwt_opt callback (tabs##update (Optdef.option id) props)

let onActivated f = addListener1 tabs##.onActivated f
let onAttached f = addListener2 tabs##.onAttached f
let onCreated f = addListener1 tabs##.onCreated f
let onDetached f = addListener2 tabs##.onDetached f
let onHighLighted f = addListener1 tabs##.onHighlighted f
let onMoved f = addListener2 tabs##.onMoved f
let onRemoved f = addListener2 tabs##.onRemoved f
let onReplaced f = addListener2 tabs##.onReplaced f
let onUpdated f = addListener3 tabs##.onUpdated f
let onZoomChanged f = addListener1 tabs##.onZoomChanged f
