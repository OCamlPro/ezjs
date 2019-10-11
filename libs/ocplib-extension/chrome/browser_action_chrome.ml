open Js_types

type uint8Array = Js_of_ocaml.Typed_array.uint8Array

class type imageData = object
  method data : uint8Array t prop
  method height : int prop
  method width : int prop
end

class type tabDetails = object
  method tabId : int optdef prop
end

class type titleDetails = object
  inherit tabDetails
  method title : js_string t opt prop
end

class type iconDetails = object
  inherit tabDetails
  method imageData : imageData t optdef prop
  method path : js_string t optdef prop
end

class type popupDetails = object
  inherit tabDetails
  method popup : js_string t opt prop
end

class type badgeDetails = object
  inherit tabDetails
  method text : js_string t opt prop
end

class type badgeColorDetails = object
  inherit tabDetails
  method color : js_string t opt prop
end

class type browserAction = object
  method setTitle : titleDetails t -> (unit -> unit) callback optdef -> unit meth
  method getTitle : tabDetails t -> (js_string t -> unit) callback -> unit meth
  method setIcon : iconDetails t -> (unit -> unit) callback optdef -> unit meth
  method setPopup : popupDetails t -> (unit -> unit) callback optdef -> unit meth
  method getPopup : tabDetails t -> (js_string t -> unit) callback -> unit meth
  method setBadgeText : badgeDetails t -> (unit -> unit) callback optdef -> unit meth
  method getBadgeText : tabDetails t -> (js_string t -> unit) callback -> unit meth
  method setBadgeBackgroundColor : badgeColorDetails t -> (unit -> unit) callback optdef -> unit meth
  method getBadgeBackgroundColor : tabDetails t -> (uint8Array t -> unit) callback -> unit meth
  method enable : int optdef -> (unit -> unit) callback optdef -> unit meth
  method disable : int optdef -> (unit -> unit) callback optdef -> unit meth
  method onClicked : Tabs_utils.tab Browser_utils.event t prop
end

let browserAction : browserAction t = variable "chrome.browserAction"

let set_title ?id ?title ?callback () =
  let details : titleDetails t = obj [||] in
  details##.title := opt string title;
  details##.tabId := def_option id;
  browserAction##setTitle details (optdef_wrap callback)

let get_title ?id f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option id;
  browserAction##getTitle details (wrap_callback (fun s -> f (to_string s)))

let set_icon ?data ?path ?id ?callback () =
  let details : iconDetails t = obj [||] in
  (match path, data with
   | Some s, _ -> details##.path := def (string s)
   | _, Some d -> details##.imageData := def d
   | _ -> ());
  details##.tabId := def_option id;
  browserAction##setIcon details (optdef_wrap callback)

let set_popup ?id ?popup ?callback () =
  let details : popupDetails t = obj [||] in
  details##.popup := opt string popup;
  details##.tabId := def_option id;
  browserAction##setPopup details (optdef_wrap callback)

let get_popup ?id f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option id;
  browserAction##getPopup details (wrap_callback (fun s -> f (to_string s)))

let set_badge ?id ?text ?callback () =
  let details : badgeDetails t = obj [||] in
  details##.text := opt string text;
  details##.tabId := def_option id;
  browserAction##setBadgeText details (optdef_wrap callback)

let get_badge ?id f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option id;
  browserAction##getBadgeText details (wrap_callback (fun s -> f (to_string s)))

let set_badge_bg ?id ?color ?callback () =
  let details : badgeColorDetails t = obj [||] in
  details##.color := opt string color;
  details##.tabId := def_option id;
  browserAction##setBadgeBackgroundColor details (optdef_wrap callback)

let get_badge_bg ?id f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option id;
  browserAction##getBadgeBackgroundColor details (wrap_callback f)

let enable ?id ?callback () =
  browserAction##enable (def_option id) (optdef_wrap callback)

let disable ?id ?callback () =
  browserAction##disable (def_option id) (optdef_wrap callback)

let onClicked f =
  Browser_utils.addListener1 browserAction##.onClicked f
