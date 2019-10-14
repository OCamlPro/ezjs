open Js_types
include Browser_action_chrome_common

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
