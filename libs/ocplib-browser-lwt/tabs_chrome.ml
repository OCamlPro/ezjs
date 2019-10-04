open Js_types
open Promise
open Browser_utils_lwt
open Runtime_utils_lwt
open Tabs_utils_lwt

class type tabs = object
  method _TAB_ID_NONE : js_string t prop
  method captureVisibleTab : int optdef -> imageDetails t optdef -> (js_string t -> unit) callback -> unit meth
  method connect : int -> 'a t optdef -> port meth
  method create : createProperties t -> (tab t -> unit) callback optdef -> unit meth
  method detectLanguage : int optdef -> (js_string t -> unit) callback -> unit meth
  method discard : int optdef -> (tab t -> unit) callback optdef -> unit meth
  method duplicate : int -> (tab t -> unit) callback optdef -> unit meth
  method executeScript : int optdef -> details t -> ('a t js_array t -> unit) callback optdef -> unit meth
  method get : int -> (tab t -> unit) callback -> unit meth
  method getCurrent : (tab t -> unit) callback -> unit meth
  method getZoom : int optdef -> (number t -> unit) callback -> unit meth
  method getZoomSettings : int optdef -> (zoomSettings t -> unit) callback -> unit meth
  method goBack : int optdef -> (unit -> unit) callback optdef -> unit meth
  method goForward : int optdef -> (unit -> unit) callback optdef -> unit meth
  method highlight : highlightInfo t -> (Dom_html.window t -> unit) callback optdef -> unit meth
  method insertCSS : int optdef -> details t meth
  method move : int -> moveProperties t -> (tab t -> unit) callback optdef -> unit meth
  method move_arr : int js_array t -> moveProperties -> (tab t js_array t -> unit) callback optdef -> unit meth
  method query : queryInfo t -> (tab t js_array t -> unit) callback -> unit meth
  method reload : int optdef -> reloadProperties t optdef -> (unit -> unit) callback optdef -> unit meth
  method remove : int -> (unit -> unit) callback optdef -> unit meth
  method remove_arr : int js_array t -> (unit -> unit) callback optdef -> unit meth
  method sendMessage : int -> 'a t -> details t optdef -> ('b t -> unit) callback optdef -> unit meth
  method setZoom : int optdef -> number t -> (unit -> unit) callback optdef -> unit meth
  method setZoomSettings : int optdef -> zoomSettings t -> (unit -> unit) callback -> unit meth
  method update : int optdef -> updateProperties t -> (tab t -> unit) callback optdef -> unit meth
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

let tabs : tabs t = Js.Unsafe.variable "chrome.tabs"
let tab_id_none () = tabs##._TAB_ID_NONE

let captureVisibleTab ?id ?options () =
  to_lwt_cb_tr to_string (fun cb ->
      tabs##captureVisibleTab
        (def_option id) (def_option options) cb)
let connect ?info id = tabs##connect id (def_option info)
let create ?callback tab =
  to_lwt_cb_opt callback (fun cb -> tabs##create tab cb)
let detectLanguage ?id () =
  to_lwt_cb (fun cb -> tabs##detectLanguage (def_option id)  cb)
let discard ?id ?callback () =
  to_lwt_cb_opt callback (fun cb -> tabs##discard (def_option id) cb)
let duplicate ?callback id =
  to_lwt_cb_opt callback (fun cb -> tabs##duplicate id cb)
let executeScript ?id ?callback details =
  to_lwt_cb_opt callback (fun cb -> tabs##executeScript (def_option id) details cb)
let get id = to_lwt_cb (fun cb -> tabs##get id cb)
let getCurrent () = to_lwt_cb (fun cb -> tabs##getCurrent cb)
let getZoom ?id () =
  to_lwt_cb_tr Js.float_of_number (fun cb -> tabs##getZoom (def_option id) cb)
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
      tabs##setZoom (def_option id) (Js.number_of_float factor) cb)
let setZoomSettings ?id props =
  to_lwt_cb (fun cb -> tabs##setZoomSettings (def_option id) props cb)
let update ?id ?callback props =
  to_lwt_cb_opt callback (fun cb -> tabs##update (def_option id) props cb)

let onActivated handler =
  tabs##.onActivated##addListener (wrap_callback handler)
let onAttached handler =
  tabs##.onAttached##addListener (wrap_callback handler)
let onCreated handler =
  tabs##.onCreated##addListener (wrap_callback handler)
let onDetached handler =
  tabs##.onDetached##addListener (wrap_callback handler)
let onHighLighted handler =
  tabs##.onHighlighted##addListener (wrap_callback handler)
let onMmoved handler =
  tabs##.onMoved##addListener (wrap_callback handler)
let onRemoved handler =
  tabs##.onRemoved##addListener (wrap_callback handler)
let onReplaced handler =
  tabs##.onReplaced##addListener (wrap_callback handler)
let onUpdated handler =
  tabs##.onUpdated##addListener (wrap_callback handler)
let onZoomChanged handler =
  tabs##.onZoomChanged##addListener (wrap_callback handler)
