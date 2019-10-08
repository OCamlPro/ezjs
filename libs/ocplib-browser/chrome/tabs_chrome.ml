open Js_types
open Browser_utils
open Runtime_utils
open Tabs_utils
module Dom_html = Js_of_ocaml.Dom_html

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

let tabs : tabs t = variable "chrome.tabs"
let tab_id_none () = tabs##._TAB_ID_NONE

let captureVisibleTab ?id ?options f =
  tabs##captureVisibleTab (def_option id) (def_option options)
    (wrap_callback (fun s -> f (to_string s)))
let connect ?info id = tabs##connect id (def_option info)
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
let insertCSS ?id () = tabs##insertCSS (def_option id)
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

let onActivated f = addListener1 tabs##.onActivated f
let onAttached f = addListener2 tabs##.onAttached f
let onCreated f = addListener1 tabs##.onCreated f
let onDetached f = addListener2 tabs##.onDetached f
let onHighLighted f = addListener1 tabs##.onHighlighted f
let onMoved f = addListener2 tabs##.onMoved f
let onRemoved f = addListener2 tabs##.onRemoved f
let onReplaced f= addListener2 tabs##.onReplaced f
let onUpdated f = addListener3 tabs##.onUpdated f
let onZoomChanged f = addListener1 tabs##.onZoomChanged f
