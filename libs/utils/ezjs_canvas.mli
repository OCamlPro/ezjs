open Js_of_ocaml

type repetition = Repeat | RepeatX | RepeatY | NoRepeat

type style =
    Color of string
  | Gradient of Dom_html.canvasGradient Js.t
  | Pattern of Dom_html.canvasPattern Js.t

type image =
    Image of Dom_html.imageElement Js.t
  | Canvas of Dom_html.canvasElement Js.t
  | Video of Dom_html.videoElement Js.t

type position = private { mutable x : float; mutable y : float; }

type t

val repetition : repetition -> Js.js_string Js.t
val create : Dom_html.document Js.t -> t
val elt : t -> Dom_html.canvasElement Js.t
val x : t -> float
val y : t -> float
val getWidth : t -> int
val getHeight : t -> int
val setWidth : t -> int -> unit
val setHeight : t -> int -> unit
val arc : t -> float -> float -> float -> float -> float -> bool -> unit
val arcTo : t -> float -> float -> float -> float -> float -> unit
val beginPath : t -> unit
val bezierCurveTo :
  t -> float -> float -> float -> float -> float -> float -> unit
val clearRect : t -> float -> float -> float -> float -> unit
val clip : t -> unit
val closePath : t -> unit
val createImageData : t -> int -> int -> Dom_html.imageData Js.t
val createLinearGradient :
  t -> float -> float -> float -> float -> Dom_html.canvasGradient Js.t
val createPattern : t -> image -> repetition -> Dom_html.canvasPattern Js.t

val createRadialGradient :
  t ->
  float ->
  float -> float -> float -> float -> float -> Dom_html.canvasGradient Js.t

val drawFocusRing :
  t -> #Dom_html.element Js.t -> float -> float -> bool -> bool Js.t

val drawImage :
  t ->
  image ->
  ?sx:float ->
  ?sy:float ->
  ?dWidth:float ->
  ?dHeight:float ->
  ?sWidth:float ->
  ?sHeight:float
  -> float
  -> float
  -> unit

val fill : t -> unit
val fillRect : t -> float -> float -> float -> float -> unit
val setFillStyle : t -> style -> unit
val fillText : t -> string -> ?maxWidth:float -> float -> float -> unit
val setFont : t -> string -> unit
val getFont : t -> Js.js_string Js.t
val getImageData :
  t -> float -> float -> float -> float -> Dom_html.imageData Js.t
val putImageData : t -> Dom_html.imageData Js.t -> float -> float -> unit
val setGlobalAlpha : t -> float -> unit
val getGlobalAlpha : t -> float
val setGlobalCompositeOperation : t -> string -> unit
val getGlobalCompositeOperation : t -> Js.js_string Js.t
val isPointInPath : t -> float -> float -> bool
val lineCap : t -> string
val lineJoin : t -> string
val lineTo : t -> float -> float -> unit
val setLineWidth : t -> float -> unit
val getLineWidth : t -> float
val measureText : t -> string -> Dom_html.textMetrics Js.t
val setMiterLimit : t -> float -> unit
val getMiterLimit : t -> float
val moveTo : t -> float -> float -> unit
val quadraticCurveTo : t -> float -> float -> float -> float -> unit
val rect : t -> float -> float -> float -> float -> unit
val restore : t -> unit
val rotate : t -> float -> unit
val save : t -> unit
val scale : t -> float -> float -> unit
val setTransform :
  t -> float -> float -> float -> float -> float -> float -> unit
val setShadowBlur : t -> float -> unit
val getShadowBlur : t -> float
val setShadowColor : t -> string -> unit
val getShadowColor : t -> string
val setShadowOffsetX : t -> float -> unit
val getShadowOffsetX : t -> float
val setShadowOffsetY : t -> float -> unit
val getShadowOffsetY : t -> float
val stroke : t -> unit
val strokeRect : t -> float -> float -> float -> float -> unit
val setStrokeStyle : t -> style -> unit
val strokeText : t -> string -> ?width:float -> float -> float -> unit
val setTextAlign : t -> string -> unit
val getTextAlign : t -> Js.js_string Js.t
val setTextBaseline : t -> string -> unit
val getTextBaseline : t -> Js.js_string Js.t
val transform :
  t -> float -> float -> float -> float -> float -> float -> unit
val translate : t -> float -> float -> unit
val addColorStop : Dom_html.canvasGradient Js.t -> float -> string -> unit
