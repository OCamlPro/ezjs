(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2015 OCamlPro: Grégoire Henry, Çağdaş Bozman.
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker (for the 'Manip' module)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

module Js = Js_of_ocaml.Js
module Url = Js_of_ocaml.Url
module Dom_html = Js_of_ocaml.Dom_html
module Firebug = Js_of_ocaml.Firebug
module File = Js_of_ocaml.File
module Dom = Js_of_ocaml.Dom
module Typed_array = Js_of_ocaml.Typed_array
module Regexp = Js_of_ocaml.Regexp

module Html = Js_of_ocaml_tyxml.Tyxml_js.Html5
module Of_dom = Js_of_ocaml_tyxml.Tyxml_js.Of_dom
module To_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom
module Xml = Js_of_ocaml_tyxml.Tyxml_js.Xml

type 'a elt = 'a Html.elt

open Js

exception JsError = Js_error.Exn
type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b

let doc = Dom_html.document
let window = Dom_html.window
(* let loc = Js.Unsafe.variable "location" *)

let alert s = window##(alert (string s))
let confirm s = to_bool (window##(confirm (string s)))

let js_log obj = Firebug.console##log obj
let js_debug obj = Firebug.console##debug obj
let js_warn obj = Firebug.console##warn obj
let js_error obj = Firebug.console##error obj

let log fmt =
  Format.kfprintf
    (fun _fmt -> Firebug.console##(log (string (Format.flush_str_formatter ()))))
    Format.str_formatter
    fmt
let debug fmt =
  Format.kfprintf
    (fun _fmt -> Firebug.console##(debug (string (Format.flush_str_formatter ()))))
    Format.str_formatter
    fmt
let warn fmt =
  Format.kfprintf
    (fun _fmt -> Firebug.console##(warn (string (Format.flush_str_formatter ()))))
    Format.str_formatter
    fmt
let error fmt =
  Format.kfprintf
    (fun _fmt -> Firebug.console##(error (string (Format.flush_str_formatter ()))))
    Format.str_formatter
    fmt

    (*
let is_hidden div =
  div##style##display = string "none"
     *)

let reload () = window##.location##reload

module Manip = struct

  let option_map f = function None -> None | Some x -> Some (f x)

  exception Error of string

  let manip_error fmt =
    Format.ksprintf
      (fun s -> debug "%s" s; raise (Error s))
      fmt

  let id x = x

  let get_node = Html.toelt

  let get_elt name elt : Dom_html.element t =
    Opt.case
      (Dom_html.CoerceTo.element (Html.toelt elt))
      (fun () ->
         manip_error
           "Cannot call %s on a node which is not an element"
           name)
      id
  let html_doc_constr : Dom_html.document constr =
    Unsafe.global##._HTMLDocument

  let document elt =
    let elt = get_elt "document" elt in
    let rec loop (elt : Dom.node t)  =
      if instanceof elt html_doc_constr
      then (Obj.magic elt : Dom_html.document t)
      else
        Opt.case
          (elt##.parentNode)
          (fun () -> (Obj.magic elt : Dom_html.document t))
          loop
    in
    loop (elt : Dom_html.element t :> Dom.node t)

  let window elt =
    let doc = document elt in
    (Obj.magic doc)##.defaultView

  let clone ?(deep=false) elt =
    let elt = get_elt "clone" elt in
    Obj.magic (elt##(cloneNode (bool deep)))

  let setInnerHtml elt s =
    let elt = get_elt "setInnerHtml" elt in
    elt##.innerHTML := string s

  let addClass elt s =
    let elt = get_elt "addClass" elt in
    elt##.classList##(add (string s))

  let removeClass elt s =
    let elt = get_elt "removeClass" elt in
    elt##.classList##(remove (string s))

  let containsClass elt s =
    let elt = get_elt "containsClass" elt in
    to_bool elt##.classList##(contains (string s))

  let setAttribute elt key value =
    let elt = get_elt "setAttribute" elt in
    elt##(setAttribute (string key) (string value))
  let removeAttribute elt key =
    let elt = get_elt "removeAttribute" elt in
    elt##(removeAttribute (string key))

  let raw_appendChild ?before node elt2 =
    match before with
    | None -> ignore(node##(appendChild (get_node elt2)))
    | Some elt3 ->
      let node3 = get_node elt3 in
      ignore(node##(insertBefore (get_node elt2) (some node3)))

  let raw_appendChildren ?before node elts =
    match before with
    | None ->
      List.iter (fun elt2 -> ignore(node##(appendChild (get_node elt2)))) elts
    | Some elt3 ->
      let node3 = get_node elt3 in
      List.iter (fun elt2 -> ignore(node##(insertBefore (get_node elt2) (some node3)))) elts

  let raw_insertChildAfter node1 node2 elt3 =
    Opt.case
      (node2##.nextSibling)
      (fun () ->
         ignore(node1##(appendChild (get_node elt3))))
      (fun node2 ->
         ignore(node1##(insertBefore (get_node elt3) (some node2))))

  let raw_insertChildrenAfter node1 node2 elts =
    Opt.case
      (node2##.nextSibling)
      (fun () ->
         List.iter (fun elt3 ->
             ignore(node1##(appendChild (get_node elt3)))))
      (fun node2 ->
         List.iter (fun elt3 ->
             ignore(node1##(insertBefore (get_node elt3) (some node2)))))
      elts

  let raw_removeChild node1 elt2 =
    let node2 = get_node elt2 in
    ignore(node1##(removeChild node2))

  let raw_replaceChild node1 elt2 elt3 =
    let node2 = get_node elt2 in
    ignore(node1##(replaceChild node2 (get_node elt3)))

  let raw_removeChildren node =
    let childrens = Dom.list_of_nodeList (node##.childNodes) in
    List.iter (fun c -> ignore(node##(removeChild c))) childrens

  let raw_replaceChildren node elts =
    raw_removeChildren node;
    List.iter (fun elt -> ignore(node##(appendChild (get_node elt)))) elts

  let nth elt n =
    let node = get_node elt in
    let res = Opt.bind (node##.childNodes##(item n)) (fun node ->
        Opt.map (Dom.CoerceTo.element node) (fun node ->
            Of_dom.of_element (Dom_html.element node)
          )
      ) in
    Opt.to_option res

  let by_id n =
    let res = Opt.bind (Dom_html.window##.document##(getElementById (string n))) (fun node ->
        Opt.map (Dom.CoerceTo.element node) (fun node ->
            Of_dom.of_element (Dom_html.element node)
          )
      ) in
    Opt.to_option res

  let by_class n =
    let nl = Dom_html.window##.document##(getElementsByClassName (string n)) in
    Array.init (nl##.length) (fun i ->
        let node = nl##(item i) in
        let node = Opt.get node (fun _ -> assert false) in
        Of_dom.of_element (Dom_html.element node)
      )
    |> Array.to_list

  let by_tag n =
    let nl = Dom_html.window##.document##(getElementsByTagName (string n)) in
    Array.init (nl##.length) (fun i ->
        let node = nl##(item i) in
        let node = Opt.get node (fun _ -> assert false) in
        Of_dom.of_element (Dom_html.element node)
      )
    |> Array.to_list


  let childLength elt =
    let node = get_node elt in
    node##.childNodes##.length

  let appendChild ?before elt1 elt2 =
    let node = get_node elt1 in
    raw_appendChild ?before node elt2

  let appendChildren ?before elt1 elts =
    let node = get_node elt1 in
    raw_appendChildren ?before node elts

  let insertChildAfter elt1 elt2 elt3 =
    let node1 = get_node elt1 in
    let node2 = get_node elt2 in
    raw_insertChildAfter node1 node2 elt3

  let insertChildrenAfter elt1 elt2 elts =
    let node1 = get_node elt1 in
    let node2 = get_node elt2 in
    raw_insertChildrenAfter node1 node2 elts

  let removeChild elt1 elt2 =
    let node1 = get_node elt1 in
    raw_removeChild node1 elt2

  let removeSelf elt =
    let node = get_node elt in
    let res = Opt.bind (node##.parentNode) (fun node ->
        Opt.map (Dom.CoerceTo.element node) (fun node ->
            Of_dom.of_element (Dom_html.element node)
          )
      ) in
    Opt.iter res (fun p -> removeChild p  elt)

  let appendChildFirst p c =
    let before = nth p 0 in
    appendChild ?before p c

  let replaceChild elt1 elt2 elt3 =
    let node1 = get_node elt1 in
    raw_replaceChild node1 elt2 elt3

  let removeChildren elt =
    let node = get_node elt in
    raw_removeChildren node

  let replaceChildren elt elts =
    let node = get_node elt in
    raw_replaceChildren node elts

  let children elt =
    let node = get_node elt in
    List.map Html.tot (Dom.list_of_nodeList (node##.childNodes))

  let parent elt =
    let node = get_node elt in
    Opt.case (node##.parentNode)
      (fun () -> None)
      (fun elt -> Some (Html.tot elt))

  let appendToBody ?before elt2 =
    let body = (Of_dom.of_body Dom_html.window##.document##.body) in
    appendChild ?before body elt2

  let get_elt_input name elt : Dom_html.inputElement t =
    Opt.case
      (Dom_html.CoerceTo.input (get_elt name elt))
      (fun () -> failwith (Printf.sprintf "Non 'input' node (%s)" name))
      id

  let get_elt_select name elt : Dom_html.selectElement t =
    Opt.case
      (Dom_html.CoerceTo.select (get_elt name elt))
      (fun () -> failwith (Printf.sprintf "Non 'select' node (%s)" name))
      id

  let get_elt_textarea name elt : Dom_html.textAreaElement t =
    Opt.case
      (Dom_html.CoerceTo.textarea (get_elt name elt))
      (fun () -> failwith (Printf.sprintf "Non element node (%s)" name))
      id

  let get_elt_img name elt : Dom_html.imageElement t =
    Opt.case
      (Dom_html.CoerceTo.img (get_elt name elt))
      (fun () -> failwith (Printf.sprintf "Non element node (%s)" name))
      id

  let scrollIntoView ?(bottom = false) elt =
    let elt = get_elt "Css.background" elt in
    elt##(scrollIntoView (bool (not bottom)))

  type disable = < disabled: bool t prop >
  let get_disable_elt name elt : disable t =
    if undefined == (Unsafe.coerce @@ Html.toelt elt)##.disabled then
      manip_error
        "Cannot call %s on a node without a 'disable' property"
        name;
    Unsafe.coerce @@ Html.toelt elt

  let disable elt =
    let elt = get_disable_elt "disable" elt in
    elt##.disabled := _true
  let enable elt =
    let elt = get_disable_elt "enable" elt in
    elt##.disabled := _false

  type focus = < focus: unit meth >
  let get_focus_elt name elt : focus t =
    if undefined == (Unsafe.coerce @@ Html.toelt elt)##.focus then
      manip_error
        "Cannot call %s on a node without a 'focus' property"
        name;
    Unsafe.coerce @@ Html.toelt elt
  let focus elt =
    let elt = get_focus_elt "focus" elt in
    elt##focus

  type blur = < blur: unit meth >
  let get_blur_elt name elt : blur t =
    if undefined == (Unsafe.coerce @@ Html.toelt elt)##.blur then
      manip_error
        "Cannot call %s on a node without a 'blur' property"
        name;
    Unsafe.coerce @@ Html.toelt elt
  let blur elt =
    let elt = get_blur_elt "blur" elt in
    elt##blur

  type value = < value: js_string t prop >
  let get_value_elt name elt : value t =
    if undefined == (Unsafe.coerce @@ Html.toelt elt)##.value then
      manip_error
        "Cannot call %s on a node without a 'value' property"
        name;
    Unsafe.coerce @@ Html.toelt elt

  let value elt =
    let elt = get_value_elt "value" elt in
    to_string elt##.value

  let set_value elt s =
    let elt = get_value_elt "value" elt in
    elt##.value := (string s)

  type files = < files: File.fileList t optdef readonly_prop >
  let get_files_elt name elt : files t =
    if undefined == (Unsafe.coerce @@ Html.toelt elt)##.files then
      manip_error
        "Cannot call %s on a node without a 'files' property"
        name;
    Unsafe.coerce @@ Html.toelt elt

  let files elt =
    let elt = get_files_elt "files" elt in
    let files = elt##.files in
    Optdef.case files
      (fun () -> [])
      (fun files ->
         let rec list_init n f = match n with
           | i when i<=0 -> []
           | i -> f (i-1) :: (list_init (i-1) f) in
         let l = list_init (files##.length) (fun i -> files##(item i)) in
         List.rev @@ List.fold_left (fun acc file ->
             Opt.case file
               (fun () -> acc)
               (fun file -> file :: acc)) [] l)

  let upload_input ?(btoa=true) ?encoding elt post =
    let files = files elt in
    List.iter (fun file ->
        let reader = new%js File.fileReader in
        reader##.onloadend :=
          Dom.handler (fun _evt ->
              if reader##.readyState = File.DONE then
                Opt.case (File.CoerceTo.string (reader##.result))
                  (fun () -> assert false)
                  (fun s ->
                     if not btoa then post (to_string s)
                     else
                       let s = to_string (Dom_html.window##(btoa s)) in
                       post s);
              _true
            );
        match encoding with
          | None -> reader##(readAsBinaryString file)
          | Some e -> reader##(readAsText_withEncoding file (Js.string e))
    ) files;
    true

  module Elt = struct
    let body =
      try Of_dom.of_body (Dom_html.window##.document##.body)
      with _ -> Obj.magic undefined (* For workers... *)
    let active () =
      (Unsafe.coerce Dom_html.window##.document)##.activeElement
  end

  module Ev = struct
    type ('a, 'b) ev = 'a Html.elt -> ('b t -> bool) -> unit
    type ('a,'b) ev_unit = 'a Html.elt -> ('b t -> unit) -> unit
    let bool_cb f = Dom_html.handler (fun e -> bool (f e))
    let onkeyup elt f =
      let elt = get_elt "Ev.onkeyup" elt in
      elt##.onkeyup := (bool_cb f)
    let onkeydown elt f =
      let elt = get_elt "Ev.onkeydown" elt in
      elt##.onkeydown := (bool_cb f)
    let onmouseup elt f =
      let elt = get_elt "Ev.onmouseup" elt in
      elt##.onmouseup := (bool_cb f)
    let onmousedown elt f =
      let elt = get_elt "Ev.onmousedown" elt in
      elt##.onmousedown := (bool_cb f)
    let onmouseout elt f =
      let elt = get_elt "Ev.onmouseout" elt in
      elt##.onmouseout := (bool_cb f)
    let onmouseover elt f =
      let elt = get_elt "Ev.onmouseover" elt in
      elt##.onmouseover := (bool_cb f)
    let onclick elt f =
      let elt = get_elt "Ev.onclick" elt in
      elt##.onclick := (bool_cb f)
    let ondblclick elt f =
      let elt = get_elt "Ev.ondblclick" elt in
      elt##.ondblclick := (bool_cb f)
    let onmousemove elt f =
      let elt = get_elt "Ev.onmousemove" elt in
      elt##.onmousemove := (bool_cb f)
    let onload elt f =
      let elt = get_elt_img "Ev.onload" elt in
      elt##.onload := (bool_cb f)
    let onerror elt f =
      let elt = get_elt_img "Ev.onerror" elt in
      elt##.onerror := (bool_cb f)
    let onabort elt f =
      let elt = get_elt_img "Ev.onabort" elt in
      elt##.onabort := (bool_cb f)
    let onfocus elt f =
      let elt = get_elt_input "Ev.onfocus" elt in
      elt##.onfocus := (bool_cb f)
    let onblur elt f =
      let elt = get_elt_input "Ev.onblur" elt in
      elt##.onblur := (bool_cb f)
    let onfocus_textarea elt f =
      let elt = get_elt_textarea "Ev.onfocus" elt in
      elt##.onfocus := (bool_cb f)
    let onblur_textarea elt f =
      let elt = get_elt_textarea "Ev.onblur" elt in
      elt##.onblur := (bool_cb f)
    let onscroll elt f =
      let elt = get_elt "Ev.onscroll" elt in
      elt##.onscroll := (bool_cb f)
    let onreturn elt f =
      let f ev =
	let key = ev##.keyCode in
	if key = 13 then f ev;
	true in
      onkeydown elt f
    let onchange elt f =
      let elt = get_elt_input "Ev.onchange" elt in
      elt##.onchange := (bool_cb f)
    let onchange_select elt f =
      let elt = get_elt_select "Ev.onchange_select" elt in
      elt##.onchange := (bool_cb f)
    let oninput elt f =
      let elt = get_elt_input "Ev.oninput" elt in
      elt##.oninput := (bool_cb f)
  end

  module Attr = struct
    let clientWidth elt =
      let elt = get_elt "Attr.clientWidth" elt in
      elt##.clientWidth
    let clientHeight elt =
      let elt = get_elt "Attr.clientHeight" elt in
      elt##.clientHeight
    let offsetWidth elt =
      let elt = get_elt "Attr.offsetWidth" elt in
      elt##.offsetWidth
    let offsetHeight elt =
      let elt = get_elt "Attr.offsetHeight" elt in
      elt##.offsetHeight
    let clientLeft elt =
      let elt = get_elt "Attr.clientLeft" elt in
      elt##.clientLeft
    let clientTop elt =
      let elt = get_elt "Attr.clientTop" elt in
      elt##.clientTop
  end

  module Css = struct
    let background elt =
      let elt = get_elt "Css.background" elt in
      to_bytestring (elt##.style##.background)
    let backgroundAttachment elt =
      let elt = get_elt "Css.backgroundAttachment" elt in
      to_bytestring (elt##.style##.backgroundAttachment)
    let backgroundColor elt =
      let elt = get_elt "Css.backgroundColor" elt in
      to_bytestring (elt##.style##.backgroundColor)
    let backgroundImage elt =
      let elt = get_elt "Css.backgroundImage" elt in
      to_bytestring (elt##.style##.backgroundImage)
    let backgroundPosition elt =
      let elt = get_elt "Css.backgroundPosition" elt in
      to_bytestring (elt##.style##.backgroundPosition)
    let backgroundRepeat elt =
      let elt = get_elt "Css.backgroundRepeat" elt in
      to_bytestring (elt##.style##.backgroundRepeat)
    let border elt =
      let elt = get_elt "Css.border" elt in
      to_bytestring (elt##.style##.border)
    let borderBottom elt =
      let elt = get_elt "Css.borderBottom" elt in
      to_bytestring (elt##.style##.borderBottom)
    let borderBottomColor elt =
      let elt = get_elt "Css.borderBottomColor" elt in
      to_bytestring (elt##.style##.borderBottomColor)
    let borderBottomStyle elt =
      let elt = get_elt "Css.borderBottomStyle" elt in
      to_bytestring (elt##.style##.borderBottomStyle)
    let borderBottomWidth elt =
      let elt = get_elt "Css.borderBottomWidth" elt in
      to_bytestring (elt##.style##.borderBottomWidth)
    let borderBottomWidthPx elt =
      let elt = get_elt "Css.borderBottomWidthPx" elt in
      parseInt (elt##.style##.borderBottomWidth)
    let borderCollapse elt =
      let elt = get_elt "Css.borderCollapse" elt in
      to_bytestring (elt##.style##.borderCollapse)
    let borderColor elt =
      let elt = get_elt "Css.borderColor" elt in
      to_bytestring (elt##.style##.borderColor)
    let borderLeft elt =
      let elt = get_elt "Css.borderLeft" elt in
      to_bytestring (elt##.style##.borderLeft)
    let borderLeftColor elt =
      let elt = get_elt "Css.borderLeftColor" elt in
      to_bytestring (elt##.style##.borderLeftColor)
    let borderLeftStyle elt =
      let elt = get_elt "Css.borderLeftStyle" elt in
      to_bytestring (elt##.style##.borderLeftStyle)
    let borderLeftWidth elt =
      let elt = get_elt "Css.borderLeftWidth" elt in
      to_bytestring (elt##.style##.borderLeftWidth)
    let borderLeftWidthPx elt =
      let elt = get_elt "Css.borderLeftWidthPx" elt in
      parseInt (elt##.style##.borderLeftWidth)
    let borderRight elt =
      let elt = get_elt "Css.borderRight" elt in
      to_bytestring (elt##.style##.borderRight)
    let borderRightColor elt =
      let elt = get_elt "Css.borderRightColor" elt in
      to_bytestring (elt##.style##.borderRightColor)
    let borderRightStyle elt =
      let elt = get_elt "Css.borderRightStyle" elt in
      to_bytestring (elt##.style##.borderRightStyle)
    let borderRightWidth elt =
      let elt = get_elt "Css.borderRightWidth" elt in
      to_bytestring (elt##.style##.borderRightWidth)
    let borderRightWidthPx elt =
      let elt = get_elt "Css.borderRightWidthPx" elt in
      parseInt (elt##.style##.borderRightWidth)
    let borderSpacing elt =
      let elt = get_elt "Css.borderSpacing" elt in
      to_bytestring (elt##.style##.borderSpacing)
    let borderStyle elt =
      let elt = get_elt "Css.borderStyle" elt in
      to_bytestring (elt##.style##.borderStyle)
    let borderTop elt =
      let elt = get_elt "Css.borderTop" elt in
      to_bytestring (elt##.style##.borderTop)
    let borderTopColor elt =
      let elt = get_elt "Css.borderTopColor" elt in
      to_bytestring (elt##.style##.borderTopColor)
    let borderTopStyle elt =
      let elt = get_elt "Css.borderTopStyle" elt in
      to_bytestring (elt##.style##.borderTopStyle)
    let borderTopWidth elt =
      let elt = get_elt "Css.borderTopWidth" elt in
      to_bytestring (elt##.style##.borderTopWidth)
    let borderTopWidthPx elt =
      let elt = get_elt "Css.borderTopWidthPx" elt in
      parseInt (elt##.style##.borderTopWidth)
    let borderWidth elt =
      let elt = get_elt "Css.borderWidth" elt in
      to_bytestring (elt##.style##.borderWidth)
    let borderWidthPx elt =
      let elt = get_elt "Css.borderWidthPx" elt in
      parseInt (elt##.style##.borderWidth)
    let borderRadius elt =
      let elt = get_elt "Css.borderRadius" elt in
      to_bytestring (elt##.style##.borderRadius)
    let bottom elt =
      let elt = get_elt "Css.bottom" elt in
      to_bytestring (elt##.style##.bottom)
    let captionSide elt =
      let elt = get_elt "Css.captionSide" elt in
      to_bytestring (elt##.style##.captionSide)
    let clear elt =
      let elt = get_elt "Css.clear" elt in
      to_bytestring (elt##.style##.clear)
    let clip elt =
      let elt = get_elt "Css.clip" elt in
      to_bytestring (elt##.style##.clip)
    let color elt =
      let elt = get_elt "Css.color" elt in
      to_bytestring (elt##.style##.color)
    let content elt =
      let elt = get_elt "Css.content" elt in
      to_bytestring (elt##.style##.content)
    let counterIncrement elt =
      let elt = get_elt "Css.counterIncrement" elt in
      to_bytestring (elt##.style##.counterIncrement)
    let counterReset elt =
      let elt = get_elt "Css.counterReset" elt in
      to_bytestring (elt##.style##.counterReset)
    let cssFloat elt =
      let elt = get_elt "Css.cssFloat" elt in
      to_bytestring (elt##.style##.cssFloat)
    let cssText elt =
      let elt = get_elt "Css.cssText" elt in
      to_bytestring (elt##.style##.cssText)
    let cursor elt =
      let elt = get_elt "Css.cursor" elt in
      to_bytestring (elt##.style##.cursor)
    let direction elt =
      let elt = get_elt "Css.direction" elt in
      to_bytestring (elt##.style##.direction)
    let display elt =
      let elt = get_elt "Css.display" elt in
      to_bytestring (elt##.style##.display)
    let emptyCells elt =
      let elt = get_elt "Css.emptyCells" elt in
      to_bytestring (elt##.style##.emptyCells)
    let font elt =
      let elt = get_elt "Css.font" elt in
      to_bytestring (elt##.style##.font)
    let fontFamily elt =
      let elt = get_elt "Css.fontFamily" elt in
      to_bytestring (elt##.style##.fontFamily)
    let fontSize elt =
      let elt = get_elt "Css.fontSize" elt in
      to_bytestring (elt##.style##.fontSize)
    let fontStyle elt =
      let elt = get_elt "Css.fontStyle" elt in
      to_bytestring (elt##.style##.fontStyle)
    let fontVariant elt =
      let elt = get_elt "Css.fontVariant" elt in
      to_bytestring (elt##.style##.fontVariant)
    let fontWeight elt =
      let elt = get_elt "Css.fontWeight" elt in
      to_bytestring (elt##.style##.fontWeight)
    let height elt =
      let elt = get_elt "Css.height" elt in
      to_bytestring (elt##.style##.height)
    let heightPx elt =
      let elt = get_elt "Css.heightPx" elt in
      parseInt (elt##.style##.height)
    let left elt =
      let elt = get_elt "Css.left" elt in
      to_bytestring (elt##.style##.left)
    let leftPx elt =
      let elt = get_elt "Css.leftPx" elt in
      parseInt (elt##.style##.left)
    let letterSpacing elt =
      let elt = get_elt "Css.letterSpacing" elt in
      to_bytestring (elt##.style##.letterSpacing)
    let lineHeight elt =
      let elt = get_elt "Css.lineHeight" elt in
      to_bytestring (elt##.style##.lineHeight)
    let listStyle elt =
      let elt = get_elt "Css.listStyle" elt in
      to_bytestring (elt##.style##.listStyle)
    let listStyleImage elt =
      let elt = get_elt "Css.listStyleImage" elt in
      to_bytestring (elt##.style##.listStyleImage)
    let listStylePosition elt =
      let elt = get_elt "Css.listStylePosition" elt in
      to_bytestring (elt##.style##.listStylePosition)
    let listStyleType elt =
      let elt = get_elt "Css.listStyleType" elt in
      to_bytestring (elt##.style##.listStyleType)
    let margin elt =
      let elt = get_elt "Css.margin" elt in
      to_bytestring (elt##.style##.margin)
    let marginBottom elt =
      let elt = get_elt "Css.marginBottom" elt in
      to_bytestring (elt##.style##.marginBottom)
    let marginBottomPx elt =
      let elt = get_elt "Css.marginBottomPx" elt in
      parseInt (elt##.style##.marginBottom)
    let marginLeft elt =
      let elt = get_elt "Css.marginLeft" elt in
      to_bytestring (elt##.style##.marginLeft)
    let marginLeftPx elt =
      let elt = get_elt "Css.marginLeftPx" elt in
      parseInt (elt##.style##.marginLeft)
    let marginRight elt =
      let elt = get_elt "Css.marginRight" elt in
      to_bytestring (elt##.style##.marginRight)
    let marginRightPx elt =
      let elt = get_elt "Css.marginRightPx" elt in
      parseInt (elt##.style##.marginRight)
    let marginTop elt =
      let elt = get_elt "Css.marginTop" elt in
      to_bytestring (elt##.style##.marginTop)
    let marginTopPx elt =
      let elt = get_elt "Css.marginTopPx" elt in
      parseInt (elt##.style##.marginTop)
    let maxHeight elt =
      let elt = get_elt "Css.maxHeight" elt in
      to_bytestring (elt##.style##.maxHeight)
    let maxHeightPx elt =
      let elt = get_elt "Css.maxHeightPx" elt in
      parseInt (elt##.style##.maxHeight)
    let maxWidth elt =
      let elt = get_elt "Css.maxWidth" elt in
      to_bytestring (elt##.style##.maxWidth)
    let maxWidthPx elt =
      let elt = get_elt "Css.maxWidthPx" elt in
      parseInt (elt##.style##.maxWidth)
    let minHeight elt =
      let elt = get_elt "Css.minHeight" elt in
      to_bytestring (elt##.style##.minHeight)
    let minHeightPx elt =
      let elt = get_elt "Css.minHeightPx" elt in
      parseInt (elt##.style##.minHeight)
    let minWidth elt =
      let elt = get_elt "Css.minWidth" elt in
      to_bytestring (elt##.style##.minWidth)
    let minWidthPx elt =
      let elt = get_elt "Css.minWidthPx" elt in
      parseInt (elt##.style##.minWidth)
    let opacity elt =
      let elt = get_elt "Css.opacity" elt in
      option_map to_bytestring (Optdef.to_option (elt##.style##.opacity))
    let outline elt =
      let elt = get_elt "Css.outline" elt in
      to_bytestring (elt##.style##.outline)
    let outlineColor elt =
      let elt = get_elt "Css.outlineColor" elt in
      to_bytestring (elt##.style##.outlineColor)
    let outlineOffset elt =
      let elt = get_elt "Css.outlineOffset" elt in
      to_bytestring (elt##.style##.outlineOffset)
    let outlineStyle elt =
      let elt = get_elt "Css.outlineStyle" elt in
      to_bytestring (elt##.style##.outlineStyle)
    let outlineWidth elt =
      let elt = get_elt "Css.outlineWidth" elt in
      to_bytestring (elt##.style##.outlineWidth)
    let overflow elt =
      let elt = get_elt "Css.overflow" elt in
      to_bytestring (elt##.style##.overflow)
    let overflowX elt =
      let elt = get_elt "Css.overflowX" elt in
      to_bytestring (elt##.style##.overflowX)
    let overflowY elt =
      let elt = get_elt "Css.overflowY" elt in
      to_bytestring (elt##.style##.overflowY)
    let padding elt =
      let elt = get_elt "Css.padding" elt in
      to_bytestring (elt##.style##.padding)
    let paddingBottom elt =
      let elt = get_elt "Css.paddingBottom" elt in
      to_bytestring (elt##.style##.paddingBottom)
    let paddingBottomPx elt =
      let elt = get_elt "Css.paddingBottomPx" elt in
      parseInt (elt##.style##.paddingBottom)
    let paddingLeft elt =
      let elt = get_elt "Css.paddingLeft" elt in
      to_bytestring (elt##.style##.paddingLeft)
    let paddingLeftPx elt =
      let elt = get_elt "Css.paddingLeftPx" elt in
      parseInt (elt##.style##.paddingLeft)
    let paddingRight elt =
      let elt = get_elt "Css.paddingRight" elt in
      to_bytestring (elt##.style##.paddingRight)
    let paddingRightPx elt =
      let elt = get_elt "Css.paddingRightPx" elt in
      parseInt (elt##.style##.paddingRight)
    let paddingTop elt =
      let elt = get_elt "Css.paddingTop" elt in
      to_bytestring (elt##.style##.paddingTop)
    let paddingTopPx elt =
      let elt = get_elt "Css.paddingTopPx" elt in
      parseInt (elt##.style##.paddingTop)
    let pageBreakAfter elt =
      let elt = get_elt "Css.pageBreakAfter" elt in
      to_bytestring (elt##.style##.pageBreakAfter)
    let pageBreakBefore elt =
      let elt = get_elt "Css.pageBreakBefore" elt in
      to_bytestring (elt##.style##.pageBreakBefore)
    let position elt =
      let elt = get_elt "Css.position" elt in
      to_bytestring (elt##.style##.position)
    let right elt =
      let elt = get_elt "Css.right" elt in
      to_bytestring (elt##.style##.right)
    let rightPx elt =
      let elt = get_elt "Css.rightPx" elt in
      parseInt (elt##.style##.right)
    let tableLayout elt =
      let elt = get_elt "Css.tableLayout" elt in
      to_bytestring (elt##.style##.tableLayout)
    let textAlign elt =
      let elt = get_elt "Css.textAlign" elt in
      to_bytestring (elt##.style##.textAlign)
    let textDecoration elt =
      let elt = get_elt "Css.textDecoration" elt in
      to_bytestring (elt##.style##.textDecoration)
    let textIndent elt =
      let elt = get_elt "Css.textIndent" elt in
      to_bytestring (elt##.style##.textIndent)
    let textTransform elt =
      let elt = get_elt "Css.textTransform" elt in
      to_bytestring (elt##.style##.textTransform)
    let top elt =
      let elt = get_elt "Css.top" elt in
      to_bytestring (elt##.style##.top)
    let topPx elt =
      let elt = get_elt "Css.topPx" elt in
      parseInt (elt##.style##.top)
    let verticalAlign elt =
      let elt = get_elt "Css.verticalAlign" elt in
      to_bytestring (elt##.style##.verticalAlign)
    let visibility elt =
      let elt = get_elt "Css.visibility" elt in
      to_bytestring (elt##.style##.visibility)
    let whiteSpace elt =
      let elt = get_elt "Css.whiteSpace" elt in
      to_bytestring (elt##.style##.whiteSpace)
    let width elt =
      let elt = get_elt "Css.width" elt in
      to_bytestring (elt##.style##.width)
    let widthPx elt =
      let elt = get_elt "Css.widthPx" elt in
      parseInt (elt##.style##.width)
    let wordSpacing elt =
      let elt = get_elt "Css.wordSpacing" elt in
      to_bytestring (elt##.style##.wordSpacing)
    let zIndex elt =
      let elt = get_elt "Css.zIndex" elt in
      to_bytestring (elt##.style##.zIndex)
  end

  module SetCss = struct
    let background elt v =
      let elt = get_elt "SetCss.background" elt in
      elt##.style##.background := bytestring v
    let backgroundAttachment elt v =
      let elt = get_elt "SetCss.backgroundAttachment" elt in
      elt##.style##.backgroundAttachment := bytestring v
    let backgroundColor elt v =
      let elt = get_elt "SetCss.backgroundColor" elt in
      elt##.style##.backgroundColor := bytestring v
    let backgroundImage elt v =
      let elt = get_elt "SetCss.backgroundImage" elt in
      elt##.style##.backgroundImage := bytestring v
    let backgroundPosition elt v =
      let elt = get_elt "SetCss.backgroundPosition" elt in
      elt##.style##.backgroundPosition := bytestring v
    let backgroundRepeat elt v =
      let elt = get_elt "SetCss.backgroundRepeat" elt in
      elt##.style##.backgroundRepeat := bytestring v
    let border elt v =
      let elt = get_elt "SetCss.border" elt in
      elt##.style##.border := bytestring v
    let borderBottom elt v =
      let elt = get_elt "SetCss.borderBottom" elt in
      elt##.style##.borderBottom := bytestring v
    let borderBottomColor elt v =
      let elt = get_elt "SetCss.borderBottomColor" elt in
      elt##.style##.borderBottomColor := bytestring v
    let borderBottomStyle elt v =
      let elt = get_elt "SetCss.borderBottomStyle" elt in
      elt##.style##.borderBottomStyle := bytestring v
    let borderBottomWidth elt v =
      let elt = get_elt "SetCss.borderBottomWidth" elt in
      elt##.style##.borderBottomWidth := bytestring v
    let borderBottomWidthPx elt v = borderBottomWidth elt (Printf.sprintf "%dpx" v)
    let borderCollapse elt v =
      let elt = get_elt "SetCss.borderCollapse" elt in
      elt##.style##.borderCollapse := bytestring v
    let borderColor elt v =
      let elt = get_elt "SetCss.borderColor" elt in
      elt##.style##.borderColor := bytestring v
    let borderLeft elt v =
      let elt = get_elt "SetCss.borderLeft" elt in
      elt##.style##.borderLeft := bytestring v
    let borderLeftColor elt v =
      let elt = get_elt "SetCss.borderLeftColor" elt in
      elt##.style##.borderLeftColor := bytestring v
    let borderLeftStyle elt v =
      let elt = get_elt "SetCss.borderLeftStyle" elt in
      elt##.style##.borderLeftStyle := bytestring v
    let borderLeftWidth elt v =
      let elt = get_elt "SetCss.borderLeftWidth" elt in
      elt##.style##.borderLeftWidth := bytestring v
    let borderLeftWidthPx elt v = borderLeftWidth elt (Printf.sprintf "%dpx" v)
    let borderRight elt v =
      let elt = get_elt "SetCss.borderRight" elt in
      elt##.style##.borderRight := bytestring v
    let borderRightColor elt v =
      let elt = get_elt "SetCss.borderRightColor" elt in
      elt##.style##.borderRightColor := bytestring v
    let borderRightStyle elt v =
      let elt = get_elt "SetCss.borderRightStyle" elt in
      elt##.style##.borderRightStyle := bytestring v
    let borderRightWidth elt v =
      let elt = get_elt "SetCss.borderRightWidth" elt in
      elt##.style##.borderRightWidth := bytestring v
    let borderRightWidthPx elt v = borderRightWidth elt (Printf.sprintf "%dpx" v)
    let borderSpacing elt v =
      let elt = get_elt "SetCss.borderSpacing" elt in
      elt##.style##.borderSpacing := bytestring v
    let borderStyle elt v =
      let elt = get_elt "SetCss.borderStyle" elt in
      elt##.style##.borderStyle := bytestring v
    let borderTop elt v =
      let elt = get_elt "SetCss.borderTop" elt in
      elt##.style##.borderTop := bytestring v
    let borderTopColor elt v =
      let elt = get_elt "SetCss.borderTopColor" elt in
      elt##.style##.borderTopColor := bytestring v
    let borderTopStyle elt v =
      let elt = get_elt "SetCss.borderTopStyle" elt in
      elt##.style##.borderTopStyle := bytestring v
    let borderTopWidth elt v =
      let elt = get_elt "SetCss.borderTopWidth" elt in
      elt##.style##.borderTopWidth := bytestring v
    let borderTopWidthPx elt v = borderTopWidth elt (Printf.sprintf "%dpx" v)
    let borderWidth elt v =
      let elt = get_elt "SetCss.borderWidth" elt in
      elt##.style##.borderWidth := bytestring v
    let borderRadius elt v =
      let elt = get_elt "SetCss.borderRadius" elt in
      elt##.style##.borderRadius := bytestring v
    let bottom elt v =
      let elt = get_elt "SetCss.bottom" elt in
      elt##.style##.bottom := bytestring v
    let bottomPx elt v = bottom elt (Printf.sprintf "%dpx" v)
    let captionSide elt v =
      let elt = get_elt "SetCss.captionSide" elt in
      elt##.style##.captionSide := bytestring v
    let clear elt v =
      let elt = get_elt "SetCss.clear" elt in
      elt##.style##.clear := bytestring v
    let clip elt v =
      let elt = get_elt "SetCss.clip" elt in
      elt##.style##.clip := bytestring v
    let color elt v =
      let elt = get_elt "SetCss.color" elt in
      elt##.style##.color := bytestring v
    let content elt v =
      let elt = get_elt "SetCss.content" elt in
      elt##.style##.content := bytestring v
    let counterIncrement elt v =
      let elt = get_elt "SetCss.counterIncrement" elt in
      elt##.style##.counterIncrement := bytestring v
    let counterReset elt v =
      let elt = get_elt "SetCss.counterReset" elt in
      elt##.style##.counterReset := bytestring v
    let cssFloat elt v =
      let elt = get_elt "SetCss.cssFloat" elt in
      elt##.style##.cssFloat := bytestring v
    let cssText elt v =
      let elt = get_elt "SetCss.cssText" elt in
      elt##.style##.cssText := bytestring v
    let cursor elt v =
      let elt = get_elt "SetCss.cursor" elt in
      elt##.style##.cursor := bytestring v
    let direction elt v =
      let elt = get_elt "SetCss.direction" elt in
      elt##.style##.direction := bytestring v
    let display elt v =
      let elt = get_elt "SetCss.display" elt in
      elt##.style##.display := bytestring v
    let emptyCells elt v =
      let elt = get_elt "SetCss.emptyCells" elt in
      elt##.style##.emptyCells := bytestring v
    let font elt v =
      let elt = get_elt "SetCss.font" elt in
      elt##.style##.font := bytestring v
    let fontFamily elt v =
      let elt = get_elt "SetCss.fontFamily" elt in
      elt##.style##.fontFamily := bytestring v
    let fontSize elt v =
      let elt = get_elt "SetCss.fontSize" elt in
      elt##.style##.fontSize := bytestring v
    let fontStyle elt v =
      let elt = get_elt "SetCss.fontStyle" elt in
      elt##.style##.fontStyle := bytestring v
    let fontVariant elt v =
      let elt = get_elt "SetCss.fontVariant" elt in
      elt##.style##.fontVariant := bytestring v
    let fontWeight elt v =
      let elt = get_elt "SetCss.fontWeight" elt in
      elt##.style##.fontWeight := bytestring v
    let height elt v =
      let elt = get_elt "SetCss.height" elt in
      elt##.style##.height := bytestring v
    let heightPx elt v = height elt (Printf.sprintf "%dpx" v)
    let left elt v =
      let elt = get_elt "SetCss.left" elt in
      elt##.style##.left := bytestring v
    let leftPx elt v = left elt (Printf.sprintf "%dpx" v)
    let letterSpacing elt v =
      let elt = get_elt "SetCss.letterSpacing" elt in
      elt##.style##.letterSpacing := bytestring v
    let lineHeight elt v =
      let elt = get_elt "SetCss.lineHeight" elt in
      elt##.style##.lineHeight := bytestring v
    let listStyle elt v =
      let elt = get_elt "SetCss.listStyle" elt in
      elt##.style##.listStyle := bytestring v
    let listStyleImage elt v =
      let elt = get_elt "SetCss.listStyleImage" elt in
      elt##.style##.listStyleImage := bytestring v
    let listStylePosition elt v =
      let elt = get_elt "SetCss.listStylePosition" elt in
      elt##.style##.listStylePosition := bytestring v
    let listStyleType elt v =
      let elt = get_elt "SetCss.listStyleType" elt in
      elt##.style##.listStyleType := bytestring v
    let margin elt v =
      let elt = get_elt "SetCss.margin" elt in
      elt##.style##.margin := bytestring v
    let marginBottom elt v =
      let elt = get_elt "SetCss.marginBottom" elt in
      elt##.style##.marginBottom := bytestring v
    let marginBottomPx elt v = marginBottom elt (Printf.sprintf "%dpx" v)
    let marginLeft elt v =
      let elt = get_elt "SetCss.marginLeft" elt in
      elt##.style##.marginLeft := bytestring v
    let marginLeftPx elt v = marginLeft elt (Printf.sprintf "%dpx" v)
    let marginRight elt v =
      let elt = get_elt "SetCss.marginRight" elt in
      elt##.style##.marginRight := bytestring v
    let marginRightPx elt v = marginRight elt (Printf.sprintf "%dpx" v)
    let marginTop elt v =
      let elt = get_elt "SetCss.marginTop" elt in
      elt##.style##.marginTop := bytestring v
    let marginTopPx elt v = marginTop elt (Printf.sprintf "%dpx" v)
    let maxHeight elt v =
      let elt = get_elt "SetCss.maxHeight" elt in
      elt##.style##.maxHeight := bytestring v
    let maxHeightPx elt v = maxHeight elt (Printf.sprintf "%dpx" v)
    let maxWidth elt v =
      let elt = get_elt "SetCss.maxWidth" elt in
      elt##.style##.maxWidth := bytestring v
    let maxWidthPx elt v = maxWidth elt (Printf.sprintf "%dpx" v)
    let minHeight elt v =
      let elt = get_elt "SetCss.minHeight" elt in
      elt##.style##.minHeight := bytestring v
    let minHeightPx elt v = minHeight elt (Printf.sprintf "%dpx" v)
    let minWidth elt v =
      let elt = get_elt "SetCss.minWidth" elt in
      elt##.style##.minWidth := bytestring v
    let minWidthPx elt v = minWidth elt (Printf.sprintf "%dpx" v)
    let opacity elt v =
      let elt = get_elt "SetCss.opacity" elt in
      elt##.style##.opacity := match v with None -> undefined | Some v -> def (bytestring v)
    let outline elt v =
      let elt = get_elt "SetCss.outline" elt in
      elt##.style##.outline := bytestring v
    let outlineColor elt v =
      let elt = get_elt "SetCss.outlineColor" elt in
      elt##.style##.outlineColor := bytestring v
    let outlineOffset elt v =
      let elt = get_elt "SetCss.outlineOffset" elt in
      elt##.style##.outlineOffset := bytestring v
    let outlineStyle elt v =
      let elt = get_elt "SetCss.outlineStyle" elt in
      elt##.style##.outlineStyle := bytestring v
    let outlineWidth elt v =
      let elt = get_elt "SetCss.outlineWidth" elt in
      elt##.style##.outlineWidth := bytestring v
    let overflow elt v =
      let elt = get_elt "SetCss.overflow" elt in
      elt##.style##.overflow := bytestring v
    let overflowX elt v =
      let elt = get_elt "SetCss.overflowX" elt in
      elt##.style##.overflowX := bytestring v
    let overflowY elt v =
      let elt = get_elt "SetCss.overflowY" elt in
      elt##.style##.overflowY := bytestring v
    let padding elt v =
      let elt = get_elt "SetCss.padding" elt in
      elt##.style##.padding := bytestring v
    let paddingBottom elt v =
      let elt = get_elt "SetCss.paddingBottom" elt in
      elt##.style##.paddingBottom := bytestring v
    let paddingBottomPx elt v = paddingBottom elt (Printf.sprintf "%dpx" v)
    let paddingLeft elt v =
      let elt = get_elt "SetCss.paddingLeft" elt in
      elt##.style##.paddingLeft := bytestring v
    let paddingLeftPx elt v = paddingLeft elt (Printf.sprintf "%dpx" v)
    let paddingRight elt v =
      let elt = get_elt "SetCss.paddingRight" elt in
      elt##.style##.paddingRight := bytestring v
    let paddingRightPx elt v = paddingRight elt (Printf.sprintf "%dpx" v)
    let paddingTop elt v =
      let elt = get_elt "SetCss.paddingTop" elt in
      elt##.style##.paddingTop := bytestring v
    let paddingTopPx elt v = paddingTop elt (Printf.sprintf "%dpx" v)
    let pageBreakAfter elt v =
      let elt = get_elt "SetCss.pageBreakAfter" elt in
      elt##.style##.pageBreakAfter := bytestring v
    let pageBreakBefore elt v =
      let elt = get_elt "SetCss.pageBreakBefore" elt in
      elt##.style##.pageBreakBefore := bytestring v
    let position elt v =
      let elt = get_elt "SetCss.position" elt in
      elt##.style##.position := bytestring v
    let right elt v =
      let elt = get_elt "SetCss.right" elt in
      elt##.style##.right := bytestring v
    let rightPx elt v = right elt (Printf.sprintf "%dpx" v)
    let tableLayout elt v =
      let elt = get_elt "SetCss.tableLayout" elt in
      elt##.style##.tableLayout := bytestring v
    let textAlign elt v =
      let elt = get_elt "SetCss.textAlign" elt in
      elt##.style##.textAlign := bytestring v
    let textDecoration elt v =
      let elt = get_elt "SetCss.textDecoration" elt in
      elt##.style##.textDecoration := bytestring v
    let textIndent elt v =
      let elt = get_elt "SetCss.textIndent" elt in
      elt##.style##.textIndent := bytestring v
    let textTransform elt v =
      let elt = get_elt "SetCss.textTransform" elt in
      elt##.style##.textTransform := bytestring v
    let top elt v =
      let elt = get_elt "SetCss.top" elt in
      elt##.style##.top := bytestring v
    let topPx elt v = top elt (Printf.sprintf "%dpx" v)
    let verticalAlign elt v =
      let elt = get_elt "SetCss.verticalAlign" elt in
      elt##.style##.verticalAlign := bytestring v
    let visibility elt v =
      let elt = get_elt "SetCss.visibility" elt in
      elt##.style##.visibility := bytestring v
    let whiteSpace elt v =
      let elt = get_elt "SetCss.whiteSpace" elt in
      elt##.style##.whiteSpace := bytestring v
    let width elt v =
      let elt = get_elt "SetCss.width" elt in
      elt##.style##.width := bytestring v
    let widthPx elt v = width elt (Printf.sprintf "%dpx" v)
    let wordSpacing elt v =
      let elt = get_elt "SetCss.wordSpacing" elt in
      elt##.style##.wordSpacing := bytestring v
    let zIndex elt v =
      let elt = get_elt "SetCss.zIndex" elt in
      elt##.style##.zIndex := bytestring v
  end

end

let hide elt = Manip.SetCss.display elt "none"

let show elt = Manip.SetCss.display elt ""

let window_open ?features url name =
  let features = match features with
    | None -> null
    | Some s -> some @@ string s in
  window##(open_ (string url) (string name) features)

module Window = struct
  let close win = win##close
  let body win = Of_dom.of_body win##.document##.body
  let head win = Of_dom.of_head win##.document##.head
  let onunload ?(win = Dom_html.window) f =
    win##.onunload := Dom_html.handler (fun ev -> bool (f ev))
  let onresize ?(win = Dom_html.window) f =
    win##.onresize := Dom_html.handler (fun ev -> bool (f ev))
  let prompt ?(win = Dom_html.window) ?(value = "") msg =
    Opt.case
      (win##(prompt (string msg) (string value)))
      (fun () -> "")
      to_string
  let onhashchange ?(win = Dom_html.window) f =
    win##.onhashchange := Dom_html.handler (fun ev -> bool (f ev))
  end


module Document = struct
  let uri () = to_string (doc##._URL)
end

let parse_fragment () =
  let elts =
    Regexp.(split (regexp "(&|%26)") (Url.Current.get_fragment ())) in
  List.fold_right
    (fun elt acc ->
       if elt = "&" || elt = "%26" || elt = "" then acc else
       match Regexp.(split (regexp "(=|%3D)") elt) with
       | [name] -> (name, "") :: acc
       | name :: _ :: value -> (name, String.concat "" value) :: acc
       | _ -> assert false)
    elts []

let set_fragment args =
  let pairs = List.map (fun (n, v) -> n ^ "=" ^ v) args in
  let fragment = String.concat "&" pairs in
  Url.Current.set_fragment fragment

let find_component id =
  match Manip.by_id id with
  | Some div -> div
  | None -> failwith ("Cannot find id " ^ id)

module Clipboard = struct
  type t = {
    mutable intercept : bool ;
    mutable data : string ;
  }

  let clipboard =
    { intercept = false ; data = "" }

  let set_copy () =
    Dom.addEventListener
      doc
      (Dom.Event.make "copy")
      (Dom.handler (fun e ->
           if clipboard.intercept then begin
             (e##.clipboardData##(setData (string "text/plain") (string clipboard.data)));
             Dom.preventDefault e;
             clipboard.intercept <- false ;
             clipboard.data <- ""
           end;
           _true))
      _true |> ignore

  let copy value : unit =
    clipboard.intercept <- true;
    clipboard.data <- value;
    Dom_html.document##(execCommand (string "copy")
                                   (_false) (null))

end
