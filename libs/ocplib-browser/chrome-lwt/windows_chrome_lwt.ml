open Js_types
open Promise_lwt
open Browser_utils

class type window = object
  method id : int optdef prop
  method focused : bool t prop
  method top : int optdef prop
  method left : int optdef prop
  method width : int optdef prop
  method height : int optdef prop
  method tabs : Tabs_utils.tab t js_array t optdef prop
  method incognito : bool t prop
  method _type : js_string t optdef prop
  method state : js_string t optdef prop
  method alwaysOnTop : bool t prop
  method sessionId : js_string t optdef prop
end

class type getInfo = object
  method populate : bool optdef prop
  method windowTypes : js_string t js_array t optdef prop
end

class type createData = object
  method url : js_string t optdef prop
  method url_arr : js_string t js_array t optdef prop
  method tabId : int optdef prop
  method left : int optdef prop
  method top : int optdef prop
  method width : int optdef prop
  method height : int optdef prop
  method focused : bool t optdef prop
  method incognito : bool t optdef prop
  method _type : js_string t optdef prop
  method state : js_string t optdef prop
  method setSelfAsOpener : bool t optdef prop
end

class type updateInfo = object
  method left : int optdef prop
  method top : int optdef prop
  method width : int optdef prop
  method height : int optdef prop
  method focused : bool t optdef prop
  method drawAttention : bool t optdef prop
  method state : js_string t optdef prop
end

class type windows = object
  method get : int -> getInfo t optdef -> (window t -> unit) callback -> unit meth
  method getCurrent : getInfo t optdef -> (window t -> unit) callback -> unit meth
  method getLastFocused : getInfo t optdef -> (window t -> unit) callback -> unit meth
  method getAll : getInfo t optdef -> (window t js_array t -> unit) callback -> unit meth
  method create : createData t optdef -> (window t -> unit) callback optdef -> unit meth
  method update : int -> updateInfo t -> (window t -> unit) callback optdef -> unit meth
  method remove : int -> (unit -> unit) callback optdef -> unit meth
  method onCreated : window t event t prop
  method onRemoved : int event t prop
  method onFocusChanged : int event t prop
end

let make_createData ?url ?url_l ?tabId ?left ?top ?width ?height ?focused ?typ
    ?state ?selfOpener () =
  let data : createData t = obj [||] in
  (match url, url_l with
   | Some _, None -> data##.url := optdef string url
   | None, Some _ ->   data##.url_arr := optdef array_of_list_str url_l
   | None, None -> ()
   | _ -> Js_log.log_str "cannot define both url and url_l for window creation");
  data##.tabId := def_option tabId;
  data##.left := def_option left;
  data##.top := def_option top;
  data##.width := def_option width;
  data##.height := def_option height;
  data##.focused := optdef bool focused;
  data##._type := optdef string typ;
  data##.state := optdef string state;
  data##.setSelfAsOpener := optdef bool selfOpener;
  data

let make_updateInfo ?left ?top ?width ?height ?focused ?drawAttention ?state () =
  let data : updateInfo t = obj [||] in
  data##.left := def_option left;
  data##.top := def_option top;
  data##.width := def_option width;
  data##.height := def_option height;
  data##.focused := optdef bool focused;
  data##.drawAttention := optdef bool drawAttention;
  data##.state := optdef string state;
  data

let windows : windows t = variable "chrome.windows"

let get ?info id =
  to_lwt_cb (fun cb -> windows##get id (def_option info) cb)
let getCurrent ?info () =
  to_lwt_cb (fun cb -> windows##getCurrent (def_option info) cb)
let getLastFocused ?info () =
  to_lwt_cb (fun cb -> windows##getLastFocused (def_option info) cb)
let getAll ?info () =
  to_lwt_cb (fun cb -> windows##getAll (def_option info) cb)
let create ?info ?callback () =
  to_lwt_cb_opt callback (fun cb -> windows##create (def_option info) cb)
let update ?callback id info =
  to_lwt_cb_opt callback (fun cb -> windows##update id info cb)
let remove ?callback id =
  to_lwt_cb_opt callback (fun cb -> windows##remove id cb)

let onCreated handler =
  windows##.onCreated##addListener handler
let onRemoved handler =
  windows##.onRemoved##addListener handler
let onFocusChanged handler =
  windows##.onFocusChanged##addListener handler
