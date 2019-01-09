open Ocp_js
open Jsbind

let jsdeps = ref [
                 "/ammap/ammap.js";
                 "/ammap/maps/js/worldLow.js";
               ]

class type areaItem =
  object
    method title : string_field
    method id : string_field
    method color : string_field
    method customData : string_field
    (* groupId can be used to group countries *)
    method groupId : string_field
    method value : int_field
  end

let item title id ?customData value =
  let obj : areaItem Js.t = Js.Unsafe.obj [||] in
  obj##title <- Js.string title;
  obj##id <- Js.string id;
  obj##customData <- Js.string (match customData with
                                | None ->  string_of_int value
                                | Some value -> value);
  obj##value <- value;
  obj

class type map_kind =
  object
  end

class type maps =
  object
    method worldLow : map_kind Js.t Js.prop
  end

class type dataProvider =
  object
    method mapVar : map_kind Js.t Js.prop
    method zoomLevel : float_field
    method zoomLongitude : int_field
    method zoomLatitude : int_field
    method areas : areaItem Js.t array_field
  end

class type areasSettings =
  object
    method unlistedAreasColor : string_field
    method rollOverOutlineColor : string_field
    method rollOverColor : string_field
    method balloonText : string_field
    method autoZoom : bool_field
  end

class type legendDataItem =
  object
    method title : string_field
    method color : string_field
  end


class type legend =
  object
    method width : int_field
    method backgroundAlpha : float_field
    method backgroundColor : string_field
    method borderColor : string_field
    method borderAlpha : float_field
    method bottom : int_field
    method left : int_field
    method right : int_field
    method horizontalGap : int_field
    method data : legendDataItem Js.t array_field
  end

class type valueLegend =
  object
    method right : int_field
    method minValue : string_field
    method maxValue : string_field

    (* not tested *)
    method width : int_field
    method backgroundAlpha : float_field
    method backgroundColor : string_field
    method borderColor : string_field
    method borderAlpha : float_field
    method bottom : int_field
    method left : int_field
    method borderThickness : float_field
    method color : string_field
    method enabled : bool_field
    method fontSize : int_field
    method height : int_field
    method left : int_field
    method showAsGradient : bool_field
    method top : int_field
  end

class type map =
  object
    method areasSettings : areasSettings Js.t Js.prop
    method creditsPosition : string_field
    method valueLegend : valueLegend Js.t Js.prop
    method dataProvider : dataProvider Js.t Js.prop
    method colorSteps : int_field
    method minValue : int_field
    method maxValue : int_field
    method addLegend : legend Js.t -> unit Js.meth
    method write : Js.js_string Js.t -> unit Js.meth
  end

class type amCharts =
  object
    method maps : maps Js.t Js.prop
    method _AmMap : map Js.t Js.constr Js.prop
    method isReady : bool_field
    method ready : (unit -> unit) Js.callback -> unit Js.meth
  end

let amCharts() =
  let amCharts : amCharts Js.t =
    Js.Unsafe.variable "AmCharts"
  in
  amCharts

let create () = jsnew ((amCharts())##_AmMap)()
let ready baseurl f =
  resolve_deps jsdeps baseurl (fun () ->
                 let amCharts = amCharts() in
                 if amCharts##isReady then
                   f ()
                 else
                   amCharts##ready (Js.wrap_callback f))

let dataProvider mapVar =
  let obj : dataProvider Js.t = Js.Unsafe.obj [||] in
  obj##mapVar <- mapVar;
  obj

let areasSettings () =
  let obj : areasSettings Js.t = Js.Unsafe.obj [||] in
  obj

let legend () =
  let obj : legend Js.t = Js.Unsafe.obj [||] in
  obj

let valueLegend () =
  let obj : valueLegend Js.t = Js.Unsafe.obj [||] in
  obj

let legendDataItem ?color title =
  let obj : legendDataItem Js.t = Js.Unsafe.obj [||] in
  obj##title <- Js.string title;
  (match color with None -> () | Some color -> obj##color <- Js.string color);
  obj
