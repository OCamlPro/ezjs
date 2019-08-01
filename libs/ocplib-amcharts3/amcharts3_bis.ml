open Ocp_js
open Jsbind

class type export = object
  method enabled : bool_field
  method divId : Js.js_string Js.t Js.opt Js.prop
end

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

class type title = object
  method alpha : int_field
  method bold : bool_field
  method color : string_field
  method id : string_field
  method size : int_field
  method tabIndex : int_field
  method text : string_field
end

class type balloon = object
  method adjustBorderColor : bool_field
  method animationDuration : float_field
  method borderAlpha : float_field
  method borderColor : string_field
  method borderThickness : int_field
  method color : string_field
  method cornerRadius : int_field
  method disableMouseEvents : bool_field
  method drop : bool_field
  method enabled : bool_field
  method fadeDuration : float_field
  method fillAlpha : float_field
  method fillColor : string_field
  method fixedPosition : bool_field
  method fontSize : int_field
  method horizontalPadding : int_field
  method maxWidth : int_field
  method offsetX : int_field
  method pointerOrientation : string_field
  method pointerWidth : int_field
  method shadowAlpha : float_field
  method shadowCOlor : string_field
  method showBullet : bool_field
  method textAlign : string_field
  method verticalPadding : int_field
end

class type label = object
  method id : string_field
  method rotation : int_field
  method size : int_field
  method tabIndex : int_field
  method text : string_field
  method url : string_field
  method x : int_field
  method y : int_field
end

class type ['a] chart = object
  method accessible : bool_field
  method accessibleDescription : string_field
  method accessibleTitle : string_field
  method addClassNames : bool_field
  method allLabel : label array_field
  method autoDisplay : bool_field
  method autoResize : bool_field
  method autoTransform : bool_field
  method backgroundAlpha : float_field
  method backgroundColor : string_field
  method balloon : balloon Js.t Js.prop
  method borderAlpha : float_field
  method borderColor : string_field
  method classNamePrefix : string_field
  method color : string_field
  method creditsPosition : string_field
  method dataProvider : 'a array_field
  method decimalSeparator : string_field
  method export : export Js.t Js.prop
  method fontFamily : string_field
  method fontSize : int_field
  method handDrawn : bool_field
  method handDrawScatter : int_field
  method handDrawThickness : int_field
  method hideBalloonTime : int_field
  method language : string_field
  method legend : legend Js.t Js.prop
  method panEventsEnabled : bool_field
  method path : string_field
  method pathToImages : string_field
  method percentPrecision : int_field
  method precision : int_field
  method processTimeout : int_field
  method svgIcons : bool_field
  method theme : string_field
  method thousandsSeparator : string_field
  method titles : title Js.t array_field
  method touchClickDuration : int_field
  method _type : string_field
  method usePrefixes : bool_field
  method addLabel :
    int -> int -> Js.js_string Js.t -> Js.js_string Js.t -> int -> Js.js_string Js.t
    -> int -> float -> bool -> Js.js_string Js.t -> unit Js.meth
  method addLegend : legend Js.t -> Js.js_string Js.t -> unit Js.meth
  (* method addListerner *)
  method addTitle : Js.js_string Js.t -> int -> Js.js_string Js.t -> float ->
    bool -> unit Js.meth
  method clear : unit Js.meth
  method clearLabels : unit Js.meth
  method invalidateSize : unit Js.meth
  method makeChart : Js.js_string Js.t -> Js.json Js.t -> int -> unit Js.meth
  method removeLegend : unit Js.meth
  (* method removeListener *)
  method validateData : unit Js.meth
  (*  method validateNow *)
  method write : Js.js_string Js.t -> unit Js.meth
end

class type axisBase = object
  method autoGridCount : bool_field
  method autoRotateAngle : int_field
  method autoRotateCOunt : int_field
  method axisAlpha : float_field
  method axisColor : string_field
  method axisThickness : int_field
  (* method axisX : int_field (\* Read-only *\)
   * method axisY : int_field (\* Read-only *\) *)
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
  method titleColor : string_field
  method titleFontSize : int_field
  method titleRotation : int_field
end

class type valueAxis = object
  inherit axisBase
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
  method synchronizeWith : valueAxis Js.t Js.prop
  method totalText : string_field
  method totalTextColor : string_field
  method totalTextOffset : int_field
  method treatZeroAs : float_field
  method _type : string_field
  method unit : string_field
  method unitPosition : string_field
  method usePrefixes : bool_field
  method useScientificNotation : bool_field
  method zeroGridAlpha : float_field
end

class type categoryAxis = object
  inherit axisBase
  method autoRotateAngle : int_field
  method autoRtateCount : int_field
  method autoWrap : bool_field
  (*  method categoryFunction : *)
  method classNameField : string_field
  (* method dateFormats *)
  method equalSpacing : bool_field
  method forceShowField : string_field
  method gridPosition : string_field
  method labelColorField : string_field
  (* method labelFunction *)
  method minPeriod : string_field
  method parseDates : bool_field
  method position : string_field
  method starOnAxis : bool_field
  method tickPosition : string_field
  method twoLineMode : bool_field
  method widthField : string_field
end

class type graph = object
  method accessibleLabel : string_field
  method alphaField : string_field
  method animationPlayed : bool_field
  method balloonColor : string_field
  method balloonText : string_field
  method behindColumns : bool_field
  method bullet : string_field
  method bulletAlpha : float_field
  method bulletAxis : valueAxis Js.t Js.prop
  method bullerBorderAlpha : float_field
  method bulletBorderColor : string_field
  method balloonText : string_field
  method bulletBorderAlpha : int_field
  method bulletBorderThickness : int_field
  method bullerColor : string_field
  method bulletField : string_field
  method bulletHitAreaSize : int_field
  method bulletOffset : int_field
  method bulletSize : int_field
  method bulletSizeField : int_field
  method classNameField : string_field
  method clustered : bool_field
  method color : string_field
  method colorField : string_field
  method columnIndexField : string_field
  method columnWidth : float_field
  method connect : bool_field
  method cornerRadiusTop : int_field
  method cursorBulletAlpha : float_field
  method customBullet : string_field
  method customBulletField : string_field
  method dashLength : int_field
  method dashLengthField : string_field
  method dateFormat : string_field
  method descriptionField : string_field
  method errorField : string_field
  method fillAlphas : float array_field
  method fillColors : Js.js_string Js.t array_field
  method fillColorsField : string_field
  method fillToAxis : valueAxis Js.t Js.prop
  method fillToGraph : graph Js.t Js.prop
  method fixedColumnWidth : int_field
  method fontSize : int_field
  method forceGap : bool_field
  method gapField : string_field
  method gapPeriod : float_field
  method gradientOrientation : string_field
  method hidden : bool_field
  method hideBulletsCount : int_field
  method highField : string_field
  method id : string_field
  method includeInMinMax : bool_field
  method labelAnchor : string_field
  method labelColorField : string_field
  method labelOffset :  int_field
  method labelPosition : string_field
  method labelRotation : int_field
  method labelText : string_field
  method legendAlpha : float_field
  method legendColor : string_field
  method legendPeriodValueText : string_field
  method legendValueText : string_field
  method lineAlpha : float_field
  method lineColor : string_field
  method lineColorField : string_field
  method lineThickness : int_field
  method lowField : string_field
  method markerType : string_field
  method maxBulletSize : int_field
  method minBulletSize : int_field
  method minDistance : int_field
  method negativeBase : int_field
  method negativeFillAlphas : float_field
  method negativeFillColors : string_field
  method negativeLineAlpha : float_field
  method negativeLineColor : string_field
  method newStack : bool_field
  method noStepRisers : bool_field
  method openField : string_field
  method periodSpan : int_field
  method pointPosition : string_field
  method precision : int_field
  method proCandlesticks : bool_field
  method showAllValueLabels : bool_field
  method showBalloon : bool_field
  method showBalloonAt : string_field
  method showBulletsAt : string_field
  method showHandOnHover : bool_field
  method showOnAxis : bool_field
  method stackable : bool_field
  method stepDirection : string_field
  method switchable : bool_field
  method tabIndex : int_field
  method title : string_field
  method topRadius : int_field
  method _type : string_field (* do not modify *)
  method urlField : string_field
  method urlTarget : string_field
  method useLineColorForBulletBorder : bool_field
  method useNegativeColorIfDown : bool_field
  method valueAxis : valueAxis Js.t Js.prop
  method valueField : string_field (* do not modify *)
  method visibleInLegend : bool_field
  method xAxis : valueAxis Js.t Js.prop
  method xField : string_field
  method yAxis : valueAxis Js.t Js.prop
  method yField : string_field
  method hideBullets : unit Js.meth
  method showBullets : unit Js.meth
end

class type chartCursor = object
  method adjustment : int_field
  method animationDuration : int_field
  method avoidBalloonOverlapping : bool_field
  method balloonPointerOrientation : string_field
  method bulletsEnabled : bool_field
  method bulletSize : int_field
  method categoryBalloonAlpha : float_field
  method categoryBalloonColor : string_field
  method categoryBalloonDateFormat : string_field
  method categoryBalloonEnabled : bool_field
  (* method categoryBalloonFunction *)
  method color : string_field
  method cursorAlpha : float_field
  method cursorColor : string_field
  method cursorPosition : string_field
  method enabled : bool_field
  method fullWidth : bool_field
  method graphBulletAlpha : float_field
  method graphBulletSize : float_field
  method leaveAfterTouch : bool_field
  method leaveCursor : bool_field
  method limitToGraph : graph Js.t Js.prop
  (* method listeners *)
  method oneBalloonOnly : bool_field
  method pan : bool_field
  method selectionAlpha : float_field
  method selectionWithoutZooming : bool_field
  method showNextAvailable : bool_field
  method tabIndex : int_field
  method valueBalloonsEnabled : bool_field
  method valueLineAlpha : float_field
  method valueLineBalloonEnabled : bool_field
  method valueLineEnabled : bool_field
  method valueZoomable : bool_field
  method zoomable : bool_field
  (* method addListener *)
  method clearSelection : unit Js.meth
  method hideCursor : unit Js.meth
  (* method removeListerner *)
  (* method showCursorAt *)
  (* method syncWithCursor *)
end

class type chartScrollbar = object
  method accessibleLable : string_field
  method autoGridCount : bool_field
  method backgroundAlpha : float_field
  method backgroundColor : string_field
  method color : string_field
  method dragCursorDown : string_field
  method dragCursorHover : string_field
  method dragIcon : string_field
  method dragIconHeight : int_field
  method dragIconWidth : int_field
  method enabled : bool_field
  method graphFillAlpha : float_field
  method graphFillColor : string_field
  method graphLineAlpha : float_field
  method graphLineColor : string_field
  method graphType : string_field
  method gridAlpha : float_field
  method gridColor : string_field
  method gridCOunt : int_field
  method hideResizeGrips : bool_field
  method hResizeCursor : string_field
  method hResizeCursorDown : string_field
  method hResizeCursorHover : string_field
  method ignoreCustomColors : bool_field
  method maximum : int_field
  method minimum : int_field
  method offset : int_field
  method oppositeAxis : bool_field
  method resizeEnabled : bool_field
  method scrollbarHeight : int_field
  method scrollDuration : int_field
  method selectedBackgroundAlpha : float_field
  method selectedBackgroundColor : string_field
  method selectedGraphFillAlpha : float_field
  method selectedGraphFillColor : string_field
  method selectedGraphLineAlpha : float_field
  method selectedGraphLineColor : string_field
  method tabIndex : int_field
  method updateOnReleaseOnly : bool_field
  method vResizeCursor : string_field
  method vResizeCursorDown : string_field
  method vResizeCursorHover : string_field
end

class type ['a] coordinateChart = object
  inherit ['a] chart
  method colors : Js.js_string Js.t array_field
  method graphs : graph Js.t array_field
  method gridAboveGraphs : bool_field
  (* method guide *)
  method sequencedAnimation : bool_field
  method startAlpha : float_field
  method startDuration : int_field
  method startEffect : string_field
  method urlTarget : string_field
  method valueAxes : valueAxis Js.t array_field
  method addGraph : graph Js.t -> unit Js.meth
  method addValueAxis : valueAxis Js.t -> unit Js.t Js.meth
  method animateAgain : unit Js.meth
  method getGraphById : Js.js_string Js.t -> graph Js.t Js.meth
  method getValueAxisById : Js.js_string Js.t -> valueAxis Js.t Js.meth
  method hideGraph : graph Js.t -> unit Js.meth
  method hideGraphBalloon : graph Js.t -> unit Js.meth
  method highlightGraph : graph Js.t -> unit Js.meth
  method removeGraph : graph Js.t -> unit Js.meth
  method removeValueAxis : valueAxis Js.t -> unit Js.meth
  method showGraph : graph Js.t -> unit Js.meth
  method showGraphsBalloone : graph Js.t -> unit Js.meth
  method unhighlightGraph : graph Js.t -> unit Js.meth
end


class type ['a] rectangularChart = object
  inherit ['a] coordinateChart
  method angle : int_field
  method autoMarginOffset : int_field
  method autoMargins : bool_field
  method chartCursor : chartCursor Js.t Js.prop
  method chartScrollbar : chartScrollbar Js.t Js.prop
  method depth3D : int_field
  method marginBottom : int_field
  method marginLeft : int_field
  method marginRight : int_field
  method marginTop : int_field
  method maxZoomFactor : int_field
  method minMarginBottom : int_field
  method minMarginLeft : int_field
  method minMarginRight : int_field
  method minMarginTop : int_field
  method plotAreaBorderAlpha : float_field
  method plotAreaBOrderColor : string_field
  method plotAreaFillAlphas : float_field
  method plotAreaFillColors : string_field
  method plotAreaGradientAngle : int_field
  (* method trendLines *)
  method zoomOutButtonAlpha : float_field
  method zoomOutButtonColor : string_field
  method zoomOutButtonImage : string_field
  method zoomOutButtonImageSize : int_field
  method zoomOutButtonPadding : int_field
  method zoomOutButtonRollOverAlpha : float_field
  method zoomOutButtonTabIndex : int_field
  method zoomOutText : string_field
  method addChartCursor : chartCursor Js.t -> unit Js.meth
  method addChartScrollbar : chartScrollbar Js.t -> unit Js.meth
  (* method addTrendLine *)
  method remveChartCursor : unit Js.meth
  method removeChartScrollbar : unit Js.meth
  (* method removeTrendLine *)
  method zoomOutValueAxes : unit Js.meth
end

class type ['a] serial = object
  inherit ['a] rectangularChart
  method balloonDateFormat : string_field
  method bezierX : int_field
  method bezierY : int_field
  method categoryAxis : categoryAxis Js.t Js.prop
  method categoryField : string_field
  method columnSpacing : int_field
  method columnSpacing3D : int_field
  method columnWidth : float_field
  method dataDateFormat : string_field
  method maxSelectedSeries : int_field
  method maxSelectedTime : int_field
  method minSelectedTime : int_field
  method mouseWheelScrollEnabled : bool_field
  method mouseWheelZoomEnabled : bool_field
  method rotate : bool_field
  method synchronizeGrid : bool_field
  method valueScrollbar : chartScrollbar Js.t Js.prop
  method zoomOutOnDataUpdate : bool_field
end

class type amCharts =
  object
    method isReady : bool_field
    method ready : (unit -> unit) Js.callback -> unit Js.meth
    method _AmSerialChart : 'a serial Js.t Js.constr Js.prop
    method _ValueAxis : valueAxis Js.t Js.constr Js.prop
    method _AmGraph : graph Js.t Js.constr Js.prop
    method _ChartCursor : chartCursor Js.t Js.constr Js.prop
    method _ChartScrollbar : chartScrollbar Js.t Js.constr Js.prop
  end

let amCharts () =
  let amCharts : amCharts Js.t = Js.Unsafe.variable "AmCharts" in
  amCharts

let export ?divId () =
  let obj : export Js.t = Js.Unsafe.obj [||] in
  obj##.enabled := true;
  obj##.divId := Js.Opt.option divId;
  obj

let create_chart () =
  let a = (amCharts ())##._AmSerialChart in
  new%js a