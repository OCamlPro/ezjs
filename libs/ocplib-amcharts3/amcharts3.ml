open Jsbind

let jsdeps = ref [
                 "/amcharts/amcharts.js";
                 "/amcharts/pie.js";
                 "/amcharts/serial.js";
                 "/amcharts/plugins/export/export.js";
                 "/amcharts/themes/light.js"
               ]

class type export =
  object
    method enabled : bool_field
    method divId : Js.js_string Js.t Js.opt Js.prop
  end

let export ?divId () =
  let obj : export Js.t = Js.Unsafe.obj [||] in
  obj##enabled <- true;
  obj##divId <- Js.Opt.option divId;
  obj

class type legend =  object
  method enabled : bool_field
  method titleText : string_field
  method valueText : string_field
  method maxColumns : int_field
end

class type titleObj = object
  method alpha : int_field
  method bold : bool_field
  (* method color : color Js.t *)
  method id : string_field
  method text : string_field
  method size : int_field
  method tabIndex : int_field
end

module Pie = struct
  class type dataItem =
    object
      method title : string_field
      method value : int_field
    end

  let dataProvider array =
    let array =
      Array.map (fun (title, value) ->
          let obj : dataItem Js.t = Js.Unsafe.obj [||] in
          obj##title <- Js.string title;
          obj##value <- value;
          obj
        ) array
    in
    Js.array array

  class type t =
    object
      method dataProvider : dataItem Js.t array_field
      method outlineColor : string_field
      method balloonText : string_field
      method outlineAlpha : float_field
      method outlineThickness : int_field
      method export : export Js.t Js.prop
      method labelsEnabled : bool_field
      method hideCredits : bool_field
      method legend : legend Js.t Js.prop
      method titles : titleObj Js.t Js.js_array Js.t Js.prop
      method theme : string_field

      method depth3D : int_field (* makes it 3D *)
      method angle : int_field   (* makes it 3D *)

      method write : Js.js_string Js.t -> unit Js.meth

      method titleField : string_field (* do not modify *)
      method valueField : string_field (* do not modify *)
    end

end

module ValueAxis = struct

  class type t =
    object
      method axisAlpha : float_field
      method inside : bool_field
      method dashLength : int_field
      method title : string_field
      method position : string_field (* "left" or "right" *)
    end
end

module Graph = struct

  class type ['dataItem] t =
    object
      method lineColor : string_field
      method negativeLineColor : string_field
      method bullet : string_field
      method bulletBorderColor : string_field
      method balloonText : string_field
      method bulletSize : int_field
      method bulletBorderAlpha : int_field
      method bulletBorderThickness : int_field
      method lineThickness : int_field
      method valueAxis : ValueAxis.t Js.t Js.prop

      method valueField : string_field (* do not modify *)
      method _type : string_field (* do not modify *)
    end

end

module ChartCursor = struct

  class type t =
    object
      method cursorAlpha : int Js.prop
      method cursorPosition : Js.js_string Js.t Js.prop
      method categoryBalloonDateFormat : Js.js_string Js.t Js.prop
    end

end

module ChartScrollbar = struct

  class type t =
    object
    end

end

class type categoryAxis =
  object
    method title : string_field
    method parseDates : bool_field
    method minPeriod : string_field
    method dashLength : int_field
    method minorGridEnabled : bool_field
    method minorGridAlpha : float_field
  end

module Serial = struct
  class type dataItem =
    object
      method x : string_field
      method y : float_field
    end

  class type dataItem2 =
    object
      method x : string_field
      method y1 : float_field
      method y2 : float_field
    end

  let dataProvider array =
    let array =
      Array.map (fun (title, value) ->
          let obj : dataItem Js.t = Js.Unsafe.obj [||] in
          obj##x <- Js.string title;
          obj##y <- value;
          obj
        ) array
    in
    Js.array array

  let dataProvider2 array =
    let array =
      Array.map (fun (title, value1, value2) ->
          let obj : dataItem2 Js.t = Js.Unsafe.obj [||] in
          obj##x <- Js.string title;
          obj##y1 <- value1;
          obj##y2 <- value2;
          obj
        ) array
    in
    Js.array array

  class type ['dataItem] t =
    object
      method dataProvider : 'dataItem Js.t Js.js_array Js.t Js.prop
      method marginLeft : int_field
      method dataDateFormat : string_field
      method creditsPosition : string_field
      method export : export Js.t Js.prop
      method categoryAxis : categoryAxis Js.t Js.prop
      method addChartCursor : ChartCursor.t Js.t -> unit Js.meth
      method addChartScrollbar : ChartScrollbar.t Js.t -> unit Js.meth
      method addGraph : 'dataItem Graph.t Js.t -> unit Js.meth
      method write : Js.js_string Js.t -> unit Js.meth
      method addValueAxis : ValueAxis.t Js.t -> unit Js.meth
      method clear : unit Js.meth
      method categoryField : string_field
    end

end

class type amCharts =
  object
    method isReady : bool_field
    method ready : (unit -> unit) Js.callback -> unit Js.meth
    method _AmPieChart : Pie.t Js.t Js.constr Js.prop
    method _AmSerialChart : 'dataItem Serial.t Js.t Js.constr Js.prop
    method _ValueAxis : ValueAxis.t Js.t Js.constr Js.prop
    method _AmGraph : 'a Graph.t Js.t Js.constr Js.prop
    method _ChartCursor : ChartCursor.t Js.t Js.constr Js.prop
    method _ChartScrollbar : ChartScrollbar.t Js.t Js.constr Js.prop
  end

let amCharts() =
  let amCharts : amCharts Js.t =
    Js.Unsafe.variable "AmCharts"
  in
  amCharts

let ready baseurl f =
  Jsbind.resolve_deps jsdeps baseurl (fun () ->
                 let amCharts = amCharts() in
                 if amCharts##isReady then
                   f ()
                 else
                   amCharts##ready (Js.wrap_callback f))

let pie () =
  let pie =  jsnew ((amCharts())##_AmPieChart) () in
  pie##titleField <- Js.string "title";
  pie##valueField <- Js.string "value";
  pie##hideCredits <- true;
  pie

let legend () =
  let legend : legend Js.t = Js.Unsafe.obj [||] in
  legend##enabled <- true ;
  legend

let title () =
  let title : titleObj Js.t = Js.Unsafe.obj [||] in
  title

let serial () =
  let chart =
    (jsnew ((amCharts())##_AmSerialChart) () : Serial.dataItem Serial.t Js.t)
  in
  chart##categoryField <- Js.string "x";
  chart

let serial2 () =
  let chart =
    (jsnew ((amCharts())##_AmSerialChart) () : Serial.dataItem2 Serial.t Js.t)
  in
  chart##categoryField <- Js.string "x";
  chart

let valueAxis () =
  jsnew ((amCharts())##_ValueAxis) ()

let chartCursor () =
  jsnew ((amCharts())##_ChartCursor) ()

let chartScrollbar () =
  jsnew ((amCharts())##_ChartScrollbar) ()

let graph _type =
  let graph = jsnew ((amCharts())##_AmGraph) () in
  graph##valueField <- Js.string "y";
  graph##_type <- Js.string _type;
  (graph : Serial.dataItem Graph.t Js.t)

let graph1_2 _type =
  let graph = jsnew ((amCharts())##_AmGraph) () in
  graph##valueField <- Js.string "y1";
  graph##_type <- Js.string _type;
  (graph : Serial.dataItem2 Graph.t Js.t)

let graph2_2 _type =
  let graph = jsnew ((amCharts())##_AmGraph) () in
  graph##valueField <- Js.string "y2";
  graph##_type <- Js.string _type;
  (graph : Serial.dataItem2 Graph.t Js.t)
