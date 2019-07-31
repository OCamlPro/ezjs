open Js_of_ocaml
open Js
open Browser_utils
open Runtime_utils
open Tabs_utils

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
  method sendMessage : int -> 'a t -> details t optdef -> (json t -> unit) callback optdef -> unit meth
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

let tabs : tabs t = Unsafe.variable "chrome.tabs"
let tab_id_none () = tabs##_TAB_ID_NONE

let captureVisibleTab ?id ?options f =
  tabs##captureVisibleTab(Optdef.option id, Optdef.option options,
                          wrap_callback (fun s -> f (to_string s)))
let connect ?info id = tabs##connect(id, Optdef.option info)
let create ?callback tab = tabs##create(tab, optdef_wrap callback)
let detectLanguage ?id f =
  tabs##detectLanguage(Optdef.option id, wrap_callback (fun s -> f (to_string s)))
let discard ?id ?callback () = tabs##discard(Optdef.option id, optdef_wrap callback)
let duplicate ?callback id = tabs##duplicate(id, optdef_wrap callback)
let executeScript ?id ?callback details =
  tabs##executeScript(Optdef.option id, details, optdef_wrap callback)
let get id f = tabs##get(id, wrap_callback f)
let getCurrent f = tabs##getCurrent(wrap_callback f)
let getZoom ?id f = tabs##getZoom(Optdef.option id, wrap_callback (fun x -> f (float_of_number x)))
let getZoomSettings ?id f = tabs##getZoomSettings(Optdef.option id, wrap_callback f)
let goBack ?id ?callback () = tabs##goBack(Optdef.option id, optdef_wrap callback)
let goForward ?id ?callback () = tabs##goForward(Optdef.option id, optdef_wrap callback)
let highlight ?callback info = tabs##highlight(info, optdef_wrap callback)
let insertCSS ?id () = tabs##insertCSS(Optdef.option id)
let move ?callback id props = tabs##move(id, props, optdef_wrap callback)
let move_list ?callback ids props = tabs##move_arr(array_of_list ids, props, optdef_wrap callback)
let query info f = tabs##query(info, wrap_callback f)
let reload ?id ?props ?callback () =
  tabs##reload(Optdef.option id, Optdef.option props, optdef_wrap callback)
let remove ?callback id = tabs##remove(id, optdef_wrap callback)
let remove_list ?callback id = tabs##remove_arr(array_of_list id, optdef_wrap callback)
let sendMessage ?details ?callback id message =
  tabs##sendMessage(id, message, Optdef.option details, optdef_wrap callback)
let setZoom ?id ?callback factor =
  tabs##setZoom(Optdef.option id, number_of_float factor, optdef_wrap callback)
let setZoomSettings ?id props f =
  tabs##setZoomSettings(Optdef.option id, props, wrap_callback f)
let update ?id ?callback props =
  tabs##update(Optdef.option id, props, optdef_wrap callback)

let onActivated handler =
  tabs##onActivated##addListener(wrap_callback handler)
let onAttached handler =
  tabs##onAttached##addListener(wrap_callback handler)
let onCreated handler =
  tabs##onCreated##addListener(wrap_callback handler)
let onDetached handler =
  tabs##onDetached##addListener(wrap_callback handler)
let onHighLighted handler =
  tabs##onHighlighted##addListener(wrap_callback handler)
let onMmoved handler =
  tabs##onMoved##addListener(wrap_callback handler)
let onRemoved handler =
  tabs##onRemoved##addListener(wrap_callback handler)
let onReplaced handler =
  tabs##onReplaced##addListener(wrap_callback handler)
let onUpdated handler =
  tabs##onUpdated##addListener(wrap_callback handler)
let onZoomChanged handler =
  tabs##onZoomChanged##addListener(wrap_callback handler)
