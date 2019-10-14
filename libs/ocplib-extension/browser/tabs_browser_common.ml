open Js_types
open Promise
open Browser_utils
open Runtime_utils
open Tabs_utils

let make_create ?active ?index ?openerTabId ?pinned ?url ?windowId () =
  let tab : createProperties t = obj [||] in
  tab##.active := optdef bool active;
  tab##.index := def_option index;
  tab##.openerTabId := def_option openerTabId;
  tab##.pinned := optdef bool pinned;
  tab##.url := optdef string url;
  tab##.windowId := def_option windowId;
  tab

class type tabs = object
  method _TAB_ID_NONE : js_string t prop
  method captureTab : int optdef -> imageDetails t optdef -> js_string t promise t meth
  method captureVisibleTab : int optdef -> imageDetails t optdef -> js_string t promise t meth
  method connect : int -> 'a t optdef -> port meth
  method create : createProperties t -> tab t promise t meth
  method detectLanguage : int optdef -> (js_string t -> unit) callback -> js_string t promise t meth
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
  method update : int optdef -> updateProperties t -> tab t promise t meth
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
let tabs : tabs t = variable "browser.tabs"
let tab_id_none () = tabs##._TAB_ID_NONE

let connect ?info id = tabs##connect id (def_option info)
let hide id = tabs##hide id
let hide_list ids = tabs##hide_arr (array_of_list ids)
let insertCSS ?id () = tabs##insertCSS (def_option id)
let moveInSuccession ?id ?options ids =
  tabs##moveInSuccession (array_of_list ids) (def_option id) (def_option options)
let print () = tabs##print
let printPreview () = tabs##printPreview
let removeCSS ?id details = tabs##removeCSS (def_option id) details
let show id = tabs##show id
let show_list ids = tabs##show_arr (array_of_list ids)
let toggleReaderMode ?id () = tabs##toggleReaderMode (def_option id)

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
