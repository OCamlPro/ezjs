open Js_types
open Promise

type uint8Array = Js_of_ocaml.Typed_array.uint8Array

class type imageData = object
  method data : uint8Array t prop
  method height : int prop
  method width : int prop
end

class type tabDetails = object
  method tabId : int t optdef prop
  method windowId : int t optdef prop
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
  method setTitle : titleDetails t -> unit t meth
  method getTitle : tabDetails t -> js_string t promise t meth
  method setIcon : iconDetails t -> unit promise t meth
  method setPopup : popupDetails t -> unit meth
  method getPopup : tabDetails t -> js_string t promise t meth
  method setBadgeText : badgeDetails t -> unit meth
  method getBadgeText : tabDetails t -> js_string t promise t meth
  method setBadgeBackgroundColor : badgeColorDetails t -> unit meth
  method getBadgeBackgroundColor : tabDetails t -> uint8Array t promise t meth
  method setBadgeTextColor : badgeColorDetails t -> unit meth
  method getBadgeTextColor : tabDetails t -> uint8Array t promise t meth
  method enable : tabDetails t -> unit meth
  method disable : tabDetails t -> unit meth
  method isEnabled : tabDetails t -> bool t promise t meth
  method onClicked : Tabs_utils.tab Browser_utils.event t prop
end

let browserAction : browserAction t = variable "chrome.browserAction"

let set_title ?tabId ?windowId ?title () =
  let details : titleDetails t = obj [||] in
  details##.title := opt string title;
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  browserAction##setTitle details

let get_title ?tabId ?windowId f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  jthen (browserAction##getTitle details) (fun s -> f (to_string s))

let set_icon ?data ?path ?tabId ?windowId () =
  let details : iconDetails t = obj [||] in
  (match path, data with
   | Some s, _ -> details##.path := def (string s)
   | _, Some d -> details##.imageData := def d
   | _ -> ());
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  browserAction##setIcon details

let set_popup ?tabId ?windowId ?popup () =
  let details : popupDetails t = obj [||] in
  details##.popup := opt string popup;
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  browserAction##setPopup details

let get_popup ?tabId ?windowId f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  jthen (browserAction##getPopup details) (fun s -> f (to_string s))

let set_badge ?tabId ?windowId ?text () =
  let details : badgeDetails t = obj [||] in
  details##.text := opt string text;
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  browserAction##setBadgeText details

let get_badge ?tabId ?windowId f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  jthen (browserAction##getBadgeText details) (fun s -> f (to_string s))

let set_badge_bg ?tabId ?windowId ?color () =
  let details : badgeColorDetails t = obj [||] in
  details##.color := opt string color;
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  browserAction##setBadgeBackgroundColor details

let get_badge_bg ?tabId ?windowId f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  jthen (browserAction##getBadgeBackgroundColor details) f

let set_badge_color ?tabId ?windowId ?color () =
  let details : badgeColorDetails t = obj [||] in
  details##.color := opt string color;
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  browserAction##setBadgeTextColor details

let get_badge_color ?tabId ?windowId f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  jthen (browserAction##getBadgeTextColor details) f

let enable ?tabId () =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option tabId;
  browserAction##enable details

let disable ?tabId () =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option tabId;
  browserAction##disable details

let isEnabled ?tabId ?windowId f =
  let details : tabDetails t = obj [||] in
  details##.tabId := def_option tabId;
  details##.windowId := def_option windowId;
  jthen (browserAction##isEnabled details) (fun b -> f (to_bool b))

let onClicked f =
  Browser_utils.addListener1 browserAction##.onClicked f
