open Js_of_ocaml
open Js

class type options = object
  method debug : bool t optdef prop
  method height : int optdef prop
  method width : int optdef prop
  method is_embed_ : bool t optdef prop
  method hash_bookmark_ : bool t optdef prop
  method default_bg_color_ : js_string t optdef prop
  method scale_factor_ : int optdef prop
  method initial_zoom_ : number t optdef prop
  method zoom_sequence_ : number t js_array t optdef prop
  method timenav_position_ : js_string t optdef prop
  method optimal_tick_width_ : int optdef prop
  method base_class_ : js_string t optdef prop
  method timenav_height_ : int optdef prop
  method timenav_height_percentage_ : int optdef prop
  method timenav_mobile_height_percentage_ : int optdef prop
  method timenav_height_min_ : int optdef prop
  method marker_height_min_ : int optdef prop
  method marker_width_min_ : int optdef prop
  method marker_padding_ : int optdef prop
  method start_at_slide_ : int optdef prop
  method menubar_height_ : int optdef prop
  method use_bc_ : bool t optdef prop
  method duration : int optdef prop
  method ease : js_string t optdef prop
  method dragging : bool t optdef prop
  method trackResize : bool t optdef prop
  method slide_padding_ : int optdef prop
  method slide_default_fade_ : js_string t optdef prop
  method language : js_string t optdef prop
  method ga_property_id_ : js_string t opt prop
  method track_events_ : js_string t js_array t optdef prop
  method script_path_ : js_string t optdef prop
end

class type timeline = object end

type timeline_cs = (js_string t -> js_string t -> options t optdef -> timeline t) constr

let timeline_cs : timeline_cs = Unsafe.variable "TL.Timeline"

let options
    ?debug ?height ?width ?is_embed ?hash_bookmark
    ?default_bg_color ?scale_factor ?initial_zoom ?zoom_sequence ?timenav_position
    ?optimal_tick_width ?base_class ?timenav_height ?timenav_height_percentage
    ?timenav_mobile_height_percentage ?timenav_height_min ?marker_height_min
    ?marker_width_min ?marker_padding ?start_at_slide ?menubar_height
    ?use_bc ?duration ?ease ?dragging ?trackResize ?slide_padding
    ?slide_default_fade ?language ?ga_property_id ?track_events ?script_path () =
  let o : options t = Unsafe.obj [||] in
  (match debug with None -> () | Some d -> o##.debug := def (bool d));
  (match height with None -> () | Some h -> o##.height := def h);
  (match width with None -> () | Some w -> o##.width := def w);
  (match is_embed with None -> () | Some e -> o##.is_embed_ := def (bool e));
  (match hash_bookmark with None -> () | Some h -> o##.hash_bookmark_ := def (bool h));
  (match default_bg_color with None -> () | Some d -> o##.default_bg_color_ := def (string d));
  (match scale_factor with None -> () | Some f -> o##.scale_factor_ := def f);
  (match initial_zoom with None -> () | Some z -> o##.initial_zoom_ := def (number_of_float z));
  (match zoom_sequence with None -> () | Some a -> o##.zoom_sequence_ := def (array @@ Array.of_list @@ List.map number_of_float a));
  (match timenav_position with None -> () | Some p -> o##.timenav_position_ := def (string p));
  (match optimal_tick_width with None -> () | Some w -> o##.optimal_tick_width_ := def w);
  (match base_class with None -> () | Some c -> o##.base_class_ := def (string c));
  (match timenav_height with None -> () | Some h -> o##.timenav_height_ := def h);
  (match timenav_height_percentage with None -> () | Some p -> o##.timenav_height_percentage_ := def p);
  (match timenav_mobile_height_percentage with None -> () | Some p -> o##.timenav_mobile_height_percentage_ := def p);
  (match timenav_height_min with None -> () | Some h -> o##.timenav_height_min_ := def h);
  (match marker_height_min with None -> () | Some h -> o##.marker_height_min_ := def h);
  (match marker_width_min with None -> () | Some w -> o##.marker_width_min_ := def w);
  (match marker_padding with None -> () | Some p -> o##.marker_padding_ := def p);
  (match start_at_slide with None -> () | Some s -> o##.start_at_slide_ := def s);
  (match menubar_height with None -> () | Some h -> o##.menubar_height_ := def h);
  (match use_bc with None -> () | Some u -> o##.use_bc_ := def (bool u));
  (match duration with None -> () | Some d -> o##.duration := def d);
  (match ease with None -> () | Some s -> o##.ease := def (string s));
  (match dragging with None -> () | Some d -> o##.dragging := def (bool d));
  (match trackResize with None -> () | Some r -> o##.trackResize := def (bool r));
  (match slide_padding with None -> () | Some p -> o##.slide_padding_ := def p);
  (match slide_default_fade with None -> () | Some s -> o##.slide_default_fade_ := def (string s));
  (match language with None -> () | Some l -> o##.language := def (string l));
  (match ga_property_id with None -> () | Some i -> o##.ga_property_id_ := some (string i));
  (match track_events with None -> () | Some t -> o##.track_events_ := def (array @@ Array.of_list @@ List.map string t));
  (match script_path with None -> () | Some p -> o##.script_path_ := def (string p));
  o

let make ?options id json =
  let options = match options with None -> undefined | Some o -> def o in
  let timeline = new%js timeline_cs (string id) (string json) options in
  export "timeline" timeline
