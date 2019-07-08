open Ocp_js
open Jsbind

class type position =
  object
    method x : int_field
    method y : int_field
  end

class type style =
  object
    method selector : string_field
    method style : 'a
  end

module DataItem =
struct
  class type scratch =
    object
      method _foo : string_field
    end

  class type data =
    object
      (* For nodes, but not edges *)
      method id : string_field
      method position : position Js.t Js.prop

      (* Fill for edges, do not fill for nodes *)
      method source : string_field
      method target : string_field
    end

  class type t =
    object
      method data : data Js.t Js.prop
      method group : string_field
    end
end

class type layout_options =
  object
    method name : string_field
  end

class type layout =
  object
    method run : unit Js.meth
  end

class type props =
  object
    method container : Dom_html.element Js.t Js.prop
    method elements : DataItem.t Js.t array_field
    method style : style Js.t array_field
    method layout : layout Js.t Js.prop
    method zoom : int_field
    method pan : position Js.t Js.prop
    method minZoom : float_field
    method maxZoom : float_field
    method zoomingEnabled : bool_field
    method userZoomingEnabled : bool_field
    method panningEnabled : bool_field
    method userPanningEnabled :bool_field
    method boxSelectionEnabled : bool_field
    method selectionType : string_field
    method touchTapThreshold : int_field
    method desktopTapThreshold : int_field
    method autoungrabify : bool_field
    method autolock : bool_field
    method headless : bool_field
    method styleEnabled : bool_field
    method hideEdgesOnViewport : bool_field
    method textureOnViewport : bool_field
    method motionBlur : bool_field
    method motionBlurOpecity : float_field
    method wheelSensitivity : float_field
  end

class type cytoscape =
  object
    method add : DataItem.t Js.t -> unit Js.meth
    method remove : DataItem.t Js.t -> unit Js.meth
    method mount : Dom_html.element Js.t -> unit Js.meth
    method layout : layout_options Js.t -> layout Js.t Js.meth

    method resize : unit Js.t Js.meth
  end

let default_style =
  let node_style : style Js.t = Js.Unsafe.obj [||] in
  node_style##selector <- Js.string "node";
  node_style##style <- Js.Unsafe.obj [|"label", ((Js.Unsafe.coerce @@ Js.string "data(id)") : Ocp_js.Js.Unsafe.any) |];
  let edge_style : style Js.t = Js.Unsafe.obj [||] in
  edge_style##selector <- Js.string "edge";
  Js.array [| node_style; edge_style |]

let position x y =
  let p : position Js.t = Js.Unsafe.obj [||] in
  p##x<-x;
  p##y<-y;
  p

let node nodename =
  let data : DataItem.data Js.t = Js.Unsafe.obj [||] in
  data##id <- Js.string nodename;

  let node : DataItem.t Js.t = Js.Unsafe.obj [||] in
  node##data <- data;
  node##group <- Js.string "nodes";
  node

let edge (source : string) (target : string) =
  let data : DataItem.data Js.t = Js.Unsafe.obj [||] in
  data##source <- Js.string source;
  data##target <- Js.string target;
  let edge : DataItem.t Js.t = Js.Unsafe.obj [||] in
  edge##group <- Js.string "edges";
  edge##data <- data;
  edge

let mk_graph ?(style= default_style) d : props Js.t =
  let g : props Js.t = Js.Unsafe.obj [||] in
  g##container <- To_dom.of_element d;
  g##style <- style;
  g

let display (props : props Js.t) =
  Js_utils.log "Casting props";
  let props = Js.Unsafe.inject props in
  Js_utils.log "Calling cytoscape";
  let g : cytoscape Js.t =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "cytoscape") [|props|] in
  Js_utils.log "Resizing the graph";
  ignore @@ g##resize ();
  Js_utils.log "Returning the graph";
  g

let add_node g nodename  =
  g##add(node nodename)

let add_edge g source target =
  g##add(edge source target)

let random_layout g : layout Js.t =
  let layout_opt : layout_options Js_of_ocaml.Js.t = Js.Unsafe.obj [||] in
  layout_opt##name <- Js.string "random";
  g##layout(layout_opt)

let run_layout (l : layout Js.t) : unit =
  l##run()
