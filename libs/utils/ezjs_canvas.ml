module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html

type repetition =
  | Repeat
  | RepeatX
  | RepeatY
  | NoRepeat

type style =
  | Color of string
  | Gradient of Dom_html.canvasGradient Js.t
  | Pattern of Dom_html.canvasPattern Js.t

type image =
  | Image of Dom_html.imageElement Js.t
  | Canvas of Dom_html.canvasElement Js.t
  | Video of Dom_html.videoElement Js.t

type position = {
  mutable x : float;
  mutable y : float
}

type t = {
  canvas : Dom_html.canvasElement Js.t;
  context2d : Dom_html.canvasRenderingContext2D Js.t;
  start_path_position: position;
  position : position;
}

(* Utils *)

let repetition r =
  Js.string (
    match r with
    | Repeat -> "repeat"
    | RepeatX -> "repeat-x"
    | RepeatY -> "repeat-y"
    | NoRepeat -> "no-repeat"
  )

(* Library *)

let create doc =
  let canvas = Dom_html.createCanvas doc in {
    canvas;
    context2d = canvas##getContext Js_of_ocaml.Dom_html._2d_;
    start_path_position = {x = nan; y = nan};
    position = {x = nan; y = nan};
  }

let elt (t : t) = t.canvas
let x (t : t) = t.position.x
let y (t : t) = t.position.y

let getWidth (t : t) : int           = t.canvas##.width
let getHeight (t : t) : int          = t.canvas##.height
let setWidth (t : t) (width : int)   = t.canvas##.width := width
let setHeight (t : t) (height : int) = t.canvas##.height := height

let arc
    (t : t)
    (x : float) (y : float) (rad : float)
    (init_angle : float) (end_angle : float) (dir : bool) =
  t.context2d##arc
    (Js.float x) (Js.float y) (Js.float rad)
    (Js.float init_angle) (Js.float end_angle) (Js.bool dir);
  let cos_end_angle = cos end_angle in
  let sin_end_angle = sin end_angle in
  let cos_end_angle_2 = cos_end_angle *. abs_float (cos_end_angle) in
  let sin_end_angle_2 = sin_end_angle *. abs_float (sin_end_angle) in
  t.position.x <- x +. rad *. cos_end_angle_2;
  t.position.y <- y -. rad *. sin_end_angle_2

let arcTo
    (t : t)
    (x0 : float) (y0 : float)
    (x1 : float) (y1 : float)
    (rad : float) =
  t.context2d##arcTo
    (Js.float x0) (Js.float y0) (Js.float x1) (Js.float y1) (Js.float rad);
  t.position.x <- nan; (* todo: calculate position *)
  t.position.y <- nan

let beginPath (t : t) = t.context2d##beginPath

let bezierCurveTo
    (t : t)
    (x0 : float) (y0 : float)
    (x1 : float) (y1 : float)
    (x2 : float) (y2 : float) =
  t.context2d##bezierCurveTo
    (Js.float x0) (Js.float y0) (Js.float x1) (Js.float y1)
    (Js.float x2) (Js.float y2);
  t.position.x <- x2;
  t.position.y <- y2

let clearRect
    (t : t)
    (x0 : float) (y0 : float)
    (x1 : float) (y1 : float) =
  t.context2d##clearRect (Js.float x0) (Js.float y0) (Js.float x1) (Js.float y1)

let clip (t : t) = t.context2d##clip

let closePath (t : t) =
  t.context2d##closePath;
  t.position.x <- t.start_path_position.x;
  t.position.y <- t.start_path_position.y

let createImageData (t : t) (width : int) (height : int) =
  t.context2d##createImageData width height

let createLinearGradient
    (t: t)
    (x0 : float) (y0 : float)
    (x1 : float) (y1 : float) =
  t.context2d##createLinearGradient
    (Js.float x0) (Js.float y0) (Js.float x1) (Js.float y1)

let createPattern
    (t : t)
    (img : image) (* Careful : must have been loaded by the browser *)
    (r : repetition) =
  let repet = repetition r in
  match img with
  | Image img -> t.context2d##createPattern            img repet
  | Canvas c  -> t.context2d##createPattern_fromCanvas c repet
  | Video vid -> t.context2d##createPattern_fromVideo  vid repet

let createRadialGradient
    (t : t)
    (x0 : float) (y0 : float) (r0 : float)
    (x1 : float) (y1 : float) (r1 : float) =
  t.context2d##createRadialGradient
    (Js.float x0) (Js.float y0) (Js.float r0)
    (Js.float x1) (Js.float y1) (Js.float r1)

let drawFocusRing
    (t : t)
    (e : (#Dom_html.element as 'a) Js.t)
    (f1 : float)
    (f2 : float)
    (b : bool) =
  t.context2d##drawFocusRing e (Js.float f1) (Js.float f2) (Js.bool b)

let drawImage
    (t : t)
    (img : image)
    ?(sx : float option)
    ?(sy : float option)
    ?(dWidth : float option)
    ?(dHeight : float option)
    ?(sWidth : float option)
    ?(sHeight : float option)
    (dx : float)
    (dy : float) =
  match dWidth, dHeight, sx, sy, sWidth, sHeight with
  | Some dWidth, Some dHeight, Some sx, Some sy, Some sWidth, Some sHeight -> begin
      match img with
      | Image img ->
        t.context2d##drawImage_full
          img (Js.float sx) (Js.float sy) (Js.float sWidth) (Js.float sHeight)
          (Js.float dx) (Js.float dy) (Js.float dWidth) (Js.float dHeight)
      | Canvas c ->
        t.context2d##drawImage_fullFromCanvas
          c (Js.float sx) (Js.float sy) (Js.float sWidth) (Js.float sHeight)
          (Js.float dx) (Js.float dy) (Js.float dWidth) (Js.float dHeight)
      | Video vid ->
        t.context2d##drawImage_fullFromVideo
          vid (Js.float sx) (Js.float sy) (Js.float sWidth) (Js.float sHeight)
          (Js.float dx) (Js.float dy) (Js.float dWidth) (Js.float dHeight)
    end

  | Some dWidth, Some dHeight, _, _, _, _ -> begin
      match img with
      | Image img ->
        t.context2d##drawImage_withSize
          img (Js.float dx) (Js.float dy) (Js.float dWidth) (Js.float dHeight)
      | Canvas c ->
        t.context2d##drawImage_fromCanvasWithSize
          c (Js.float dx) (Js.float dy) (Js.float dWidth) (Js.float dHeight)
      | Video vid ->
        t.context2d##drawImage_fromVideoWithSize
          vid (Js.float dx) (Js.float dy) (Js.float dWidth) (Js.float dHeight)
    end
  | _ -> begin
      match img with
      | Image img ->
        t.context2d##drawImage img (Js.float dx) (Js.float dy)
      | Canvas c ->
        t.context2d##drawImage_fromCanvas c (Js.float dx) (Js.float dy)
      | Video vid ->
        t.context2d##drawImage_fromVideoWithVideo vid (Js.float dx) (Js.float dy)
    end

let fill (t : t) = t.context2d##fill

let fillRect (t : t) (x0 : float) (y0 : float) (x1 : float) (y1 : float) =
  t.context2d##fillRect (Js.float x0) (Js.float y0) (Js.float x1) (Js.float y1)

let setFillStyle (t : t) (s : style) =
  match s with
  | Color s -> t.context2d##.fillStyle := Js.string s
  | Gradient g -> t.context2d##.fillStyle_gradient := g
  | Pattern p -> t.context2d##.fillStyle_pattern := p

let fillText (t : t) (text : string) ?(maxWidth: float option) (x : float) (y : float) =
  match maxWidth with
  | None -> t.context2d##fillText (Js.string text) (Js.float x) (Js.float y)
  | Some w ->
    t.context2d##fillText_withWidth (Js.string text) (Js.float x) (Js.float y)
      (Js.float w)

let setFont (t : t) (font : string) =
  t.context2d##.font := Js.string font

let getFont (t : t) = t.context2d##.font

let getImageData (t : t) (sx : float) (sy : float) (sw : float) (sh : float) =
  t.context2d##getImageData (Js.float sx) (Js.float sy) (Js.float sw) (Js.float sh)

let putImageData (t : t) (img : Dom_html.imageData Js.t) (dx: float) (dy : float) =
  t.context2d##putImageData img (Js.float dx) (Js.float dy)

let setGlobalAlpha (t : t) (f : float) = t.context2d##.globalAlpha := Js.float f
let getGlobalAlpha (t : t) = Js.to_float t.context2d##.globalAlpha

let setGlobalCompositeOperation (t : t) (op : string) =
  t.context2d##.globalCompositeOperation := (Js.string op)
let getGlobalCompositeOperation (t : t) =
  t.context2d##.globalCompositeOperation

let isPointInPath (t : t) (x : float) (y : float) =
  Js.to_bool (t.context2d##isPointInPath (Js.float x) (Js.float y))

let lineCap (t : t) =
  Js.to_string t.context2d##.lineCap

let lineJoin (t : t) =
  Js.to_string t.context2d##.lineJoin

let lineTo (t : t) (x : float) (y : float) =
  t.context2d##lineTo (Js.float x) (Js.float y);
  t.position.x <- x;
  t.position.y <- y

let setLineWidth (t : t) (w : float) = t.context2d##.lineWidth := Js.float w
let getLineWidth (t : t) = Js.to_float t.context2d##.lineWidth

let measureText (t : t) (txt : string) =
  t.context2d##measureText (Js.string txt)

let setMiterLimit (t : t) (f : float) = t.context2d##.miterLimit := Js.float f
let getMiterLimit (t : t) = Js.to_float t.context2d##.miterLimit

let moveTo (t : t) (x : float) (y : float) =
  t.context2d##moveTo (Js.float x) (Js.float y);
  t.position.x <- x;
  t.position.y <- y

let quadraticCurveTo (t : t) (x1 : float) (y1 : float) (x2 : float) (y2 : float) =
  t.context2d##quadraticCurveTo
    (Js.float x1) (Js.float y1) (Js.float x2) (Js.float y2);
  t.position.x <- x2;
  t.position.y <- y2

let rect (t : t) (x0 : float) (y0 : float) (x1 : float) (y1 : float) =
  t.context2d##rect (Js.float x0) (Js.float y0) (Js.float x1) (Js.float y1)

let restore (t : t) = t.context2d##restore

let rotate (t : t) (deg : float) = t.context2d##rotate (Js.float deg)

let save (t : t) = t.context2d##save

let scale (t : t) (x : float) (y : float) =
  t.context2d##scale (Js.float x) (Js.float y);
  t.position.x <- t.position.x *. x;
  t.position.y <- t.position.y *. y

let setTransform
    (t : t)
    (a : float) (b : float) (c : float)
    (d : float) (e : float) (f : float) =
  t.context2d##setTransform
    (Js.float a) (Js.float b) (Js.float c) (Js.float d) (Js.float e) (Js.float f)

let setShadowBlur (t : t) (blur : float) = t.context2d##.shadowBlur := Js.float blur
let getShadowBlur (t : t) = Js.to_float t.context2d##.shadowBlur

let setShadowColor (t : t) (color : string) = t.context2d##.shadowColor := Js.string color
let getShadowColor (t : t) = Js.to_string t.context2d##.shadowColor

let setShadowOffsetX (t : t) (x : float) = t.context2d##.shadowOffsetX := Js.float x
let getShadowOffsetX (t : t) = Js.to_float t.context2d##.shadowOffsetX

let setShadowOffsetY (t : t) (y : float) = t.context2d##.shadowOffsetY := Js.float y
let getShadowOffsetY (t : t) = Js.to_float t.context2d##.shadowOffsetY

let stroke (t : t) = t.context2d##stroke

let strokeRect (t : t) (x0 : float) (y0 : float) (x1 : float) (y1 : float) =
  t.context2d##strokeRect (Js.float x0) (Js.float y0) (Js.float x1) (Js.float y1)

let setStrokeStyle (t : t) (style : style) =
  match style with
  | Color s -> t.context2d##.strokeStyle := Js.string s
  | Gradient g -> t.context2d##.strokeStyle_gradient := g
  | Pattern p -> t.context2d##.strokeStyle_pattern := p

let strokeText
    (t : t)
    (txt : string) ?(width : float option)
    (x : float) (y : float) =
  match width with
  | None -> t.context2d##strokeText (Js.string txt) (Js.float x) (Js.float y)
  | Some w ->
    t.context2d##strokeText_withWidth
      (Js.string txt) (Js.float x) (Js.float y) (Js.float w)

let setTextAlign (t : t) (styl : string) =
  t.context2d##.textAlign := Js.string styl
let getTextAlign (t : t) = t.context2d##.textAlign

let setTextBaseline (t : t) (styl : string) =
  t.context2d##.textBaseline := Js.string styl
let getTextBaseline (t : t) = t.context2d##.textBaseline

let transform
    (t : t)
    (a : float) (b : float) (c : float)
    (d : float) (e : float) (f : float) =
  t.context2d##transform
    (Js.float a) (Js.float b) (Js.float c) (Js.float d) (Js.float e) (Js.float f)

let translate (t : t) (x : float) (y : float) =
  t.context2d##translate (Js.float x) (Js.float y)

let addColorStop (g : Dom_html.canvasGradient Js.t) (stop : float) (color : string) =
  g##addColorStop (Js.float stop) (Js.string color)
