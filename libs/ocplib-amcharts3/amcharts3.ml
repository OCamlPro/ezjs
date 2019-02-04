open Ocp_js
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
  method accessibleLabel : string_field
  method align : string_field
  method autoMargins : bool_field
  method backgroundAlpha : int_field
  method backgroundColor : string_field
  method borderAlpha : int_field
  method borderColor : string_field
  method bottom : int_field
  method color : string_field
  method combineLegend : bool_field
  method divId : string_field
  method enabled : bool_field
  method equalWidths : bool_field
  method fontSize : int_field
  method forceWidth : bool_field
  method gradientRotation : int_field
  method horizontalGap : int_field
  method labelWidth : int_field
  method left : int_field
  method marginBottom : int_field
  method marginLeft : int_field
  method marginRight : int_field
  method marginTop : int_field
  method markerBorderAlpha : int_field
  method markerBorderColor : string_field
  method markerBorderThickness : int_field
  method markerDisabledColor : string_field
  method markerLabelGap : int_field
  method markerSize : int_field
  method markerType : string_field
  method maxColumns : int_field
  method periodValueText : string_field
  method position : string_field
  method reversedOrder : bool_field
  method right : int_field
  method rollOverColor : string_field
  method rollOverGraphAlpha : int_field
  method showEntries : bool_field
  method spacing : int_field
  method switchable : bool_field
  method switchColor : string_field
  method switchType : string_field
  method tabIndex : int_field
  method textClickEnabled : bool_field
  method top : int_field
  method useGraphSettings : bool_field
  method valueAlign : string_field
  method valueText : string_field
  method valueWidth : int_field
  method verticalGap : int_field
  method width : int_field
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
      method addClassNames: bool_field
    end

end

module AxisBase =
  struct
    class type t =
      object
        method autoGridCount : bool_field
        method autoRotateAngle : int_field
        method autoRotateCOunt : int_field
        method axisAlpha : float_field
        method axisColor : string_field
        method axisThickness : int_field
        method axisX : int_field (* Read-only *)
        method axisY : int_field (* Read-only *)
        (* method balloon : AmBalloon.t TODO *)
        method boldLabels : bool_field
        method boldPeriodBeginning : bool_field
        method centerLabelOnFullPeriod : bool_field
        method centerLabels : bool_field
        method centerRotatedLabels : bool_field
        method color : string_field
        method dashLength : int_field
        (* method dateFormats : date array_field TODO *)
        method fillAlpha : int_field
        method fillColor : string_field
        method firstDayOfWeek : int_field
        method fontSize : int_field
        method gridAlpha : float_field
        method gridColor : string_field
        method gridCount : int_field
        method gridThickness : int_field
        (* method guides : Guide.t array_field TODO *)
        method ignoreAxisWidth : bool_field
        method inside : bool_field
        method labelFrequency : int_field
        method labelOffset : int_field
        method labelRotation : int_field
        method labelsEnables : bool_field
        (* method listeners : object array_field TODO *)
        method markPeriodChange : bool_field
        method minHorizontalGap : int_field
        method minorGridAlpha: float_field
        method minorGridEnabled : bool_field
        method minorTickLength : int_field
        method minVerticalGap : int_field
        method offset : int_field
        method position : string_field (* "left" or "right" *)
        method showFirstLabel : bool_field
        method showLastLabel : bool_field
        method tickLength : int_field
        method title : string_field
        method titleBold : bool_field
        method titleFontSize : int_field
        method titleColor : string_field
        method titleRotation : int_field
      end
  end

module ValueAxis = struct

  class type t =
    object
      inherit AxisBase.t
      method autoWrap : bool_field
      method axisFrequency : int_field
      method axisTitleOffset : int_field
      (* method balloonTextFunction : object TODO *)
      method baseCoord : int_field
      method baseValue : int_field
      method duration : string_field
      (* method durationUnits : object TODO*)
      method gridType : string_field
      method id : string_field
      method includeAllValues : bool_field
      method includeGuidesInMinMax : bool_field
      method includeHidden : bool_field
      method integersOnly : bool_field
      (*method labelFunction : ? TODO *)
      method logarithmic : bool_field
      method max : int_field (* Read-only *)
      method maximum : int_field
      (* method maximumDate : Date.t TODO *)
      method min : int_field (* Read-only *)
      method minimum : int_field
      (* method minimumDate : Date.t TODO *)
      method minMaxMultiplier : float_field
      method minPeriod : string_field
      method pointPosition : string_field
      method position : string_field
      method precision : int_field
      method radarCategoriesEnabled : bool_field
      method recalculateToPercents : bool_field
      method reversed : bool_field
      method stackType : string_field
      method step : int_field (* Read-only *)
      method strictMinMax : bool_field
      method synchronizationMultiplier : int_field
      method synchronizeWith : t
      method totalText : string_field
      method totalTextColor : string_field
      method totalTextOffset : int_field
      method treatZeroAs : float_field
      (* method type : string_field  // "type" is an OCaml key word*)
      method unit : string_field
      method unitPosition : string_field
      method usePrefixes : bool_field
      method useScientificNotation : bool_field
      method zeroGridAlpha : float_field
    end
end

module CategoryAxis =
  struct
    class type t =
      object
        inherit AxisBase.t
        method minPeriod : string_field
        method parseDates : bool_field
      end
  end

class type categoryAxis = CategoryAxis.t

module DataItem = struct

  class type t =
    object
      method x : string_field
      method y : (string * float_field) array_field
    end

  let dataProvider array =
    let new_array =
      Array.map
        (fun (title, value_array) ->
           let obj : t Js.t =
             Js.Unsafe.obj @@ Array.map
               (fun (field_name,field_val) ->
                  field_name,Js.Unsafe.inject field_val)
               value_array
           in
           obj##x <- Js.string title;
           obj
        ) array
    in
    Js.array new_array

end

module Graph = struct

  class type t =
    object
      method dataProvider : DataItem.t Js.t array_field
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
      method title : string_field
      method dashLength : int_field

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

module Serial = struct

  let dataProvider array =
    DataItem.dataProvider @@
    Array.map (fun (title,value) -> (title,[|"y",value|])) array

  let dataProvider2 array =
    DataItem.dataProvider @@
    Array.map (fun (title,v1,v2) -> (title,[|"y1",v1;"y2",v2|])) array

  class type t =
    object
      method dataProvider : DataItem.t Js.t Js.js_array Js.t Js.prop
      method marginLeft : int_field
      method dataDateFormat : string_field
      method creditsPosition : string_field
      method export : export Js.t Js.prop
      method categoryAxis : categoryAxis Js.t Js.prop
      method addChartCursor : ChartCursor.t Js.t -> unit Js.meth
      method addChartScrollbar : ChartScrollbar.t Js.t -> unit Js.meth
      method addGraph : Graph.t Js.t -> unit Js.meth
      method write : Js.js_string Js.t -> unit Js.meth
      method addValueAxis : ValueAxis.t Js.t -> unit Js.meth
      method clear : unit Js.meth
      method categoryField : string_field
      method legend : legend Js.t Js.prop
      method addClassNames : bool_field
    end

end

class type amCharts =
  object
    method isReady : bool_field
    method ready : (unit -> unit) Js.callback -> unit Js.meth
    method _AmPieChart : Pie.t Js.t Js.constr Js.prop
    method _AmSerialChart : Serial.t Js.t Js.constr Js.prop
    method _ValueAxis : ValueAxis.t Js.t Js.constr Js.prop
    method _AmGraph : Graph.t Js.t Js.constr Js.prop
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

let serialN () =
  let chart =
    (jsnew ((amCharts())##_AmSerialChart) () : Serial.t Js.t)
  in
  chart##categoryField <- Js.string "x";
  chart

let serial = serialN

let serial2 = serialN

let valueAxis () =
  jsnew ((amCharts())##_ValueAxis) ()

let chartCursor () =
  jsnew ((amCharts())##_ChartCursor) ()

let chartScrollbar () =
  jsnew ((amCharts())##_ChartScrollbar) ()

let graphN field _type =
  let graph = jsnew ((amCharts())##_AmGraph) () in
  graph##valueField <- Js.string field;
  graph##_type <- Js.string _type;
  (graph : Graph.t Js.t)

let graph = graphN "y"

let graph1_2 = graphN "y1"

let graph2_2 = graphN "y2"
