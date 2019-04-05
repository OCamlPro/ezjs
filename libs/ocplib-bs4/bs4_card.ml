open Ocp_js
open Html
open Bs4.Utils
open Flex
open Display
open Spacing
open Attribute
open Bs4.Items
open BCard
open Table
open Dropdown
open Button
open Pagination
open Popover
open Spinner

(* Pagination *)

let popover_link ?(classes=[]) ?(prefix="") value =
  let classes = page_link :: classes in
  let create_id = let cpt = ref 0 in
    fun () -> incr cpt; prefix ^ "goto-page-" ^ (string_of_int !cpt) in
  let id = create_id () in
  make_popover ~classes id value, id

let mk_pages ?(use_sep=false) mk_page range =
  let rec mk rem =
    match rem with
    | i :: j :: rem ->
      if j - i <> 1 then
        if use_sep then mk_page i :: (None, false) :: mk (j :: rem)
        else mk_page i :: mk (j :: rem)
      else mk_page i :: mk (j :: rem)
    | i :: [] -> [ mk_page i ]
    | [] -> []
  in
  mk range

let mk_range ?(accel=10) page page_total =
  let last_page = page_total - 1 in
  let last_pages =
    if page >= last_page then []
    else if accel < 2 || page + accel >= last_page then [ last_page ]
    else [ page + accel; last_page ] in
  let next_page =
    if page + 1 < last_page then page + 1 :: last_pages else last_pages in
  let prev_page =
    if page - 1 < 0 then page :: next_page
    else (page - 1) :: page :: next_page in
  let prev_minus_accel =
    if page <= accel || accel < 2 then prev_page else
      (page - accel) :: prev_page in
  match prev_minus_accel with
  | hd :: _ -> if hd > 0 then 0 :: prev_minus_accel else prev_minus_accel
  | [] -> assert false

let page_range page page_total =
  let make_page i = Some i, i = page in
  if page_total <= 5 then List.init page_total make_page
  else mk_pages ~use_sep:true make_page (mk_range ~accel:0 page page_total)

let make_paginate ?(classes=[]) ?(prefix="") page page_size size seps f =
  let mk_onclick_page page _ = f page page_size; true in
  let nb_pages = max 1 ( ((size - 1) / page_size + 1)) in
  let pages = page_range page nb_pages in
  if nb_pages <= 1 then span ~a:[a_class ["hidden"]] []
  else (
    let items = List.map (function
        | Some p, true ->
          make_page_item ~item_class:["active"] ~aclass:classes
            (txt @@ Printf.sprintf "%d" (p+1))
        | Some p, false ->
          make_page_item
            ~onclick:(mk_onclick_page p)
            ~aclass:(["hidden-xs"; "hidden-sm"] @ classes)
            (txt @@ Printf.sprintf "%d" (p+1))
        | None, _ ->
          let link, id = popover_link ~classes ~prefix "..." in
          seps := id :: !seps;
          li ~a:[a_class ["hidden-xs"; "hidden-sm"]] [ link ]
      ) pages in
    let prev =
      if page = 0 then
        make_page_item ~item_class:["disabled"] ~aclass:classes (txt "«")
      else
        make_page_item ~aclass:classes ~onclick:(mk_onclick_page (page-1)) (txt "«") in
    let next =
      if nb_pages = page + 1 then
        make_page_item ~aclass:classes ~item_class:["disabled"] (txt "»")
      else
        make_page_item ~aclass:classes ~onclick:(mk_onclick_page (page+1)) (txt "»") in
    let mobile_select =
      let link, id = popover_link ~classes ~prefix "..." in
      seps := id :: !seps;
      li ~a:[a_class ["hidden-lg"; "hidden-xl"] ] [ link ] in
    ul ~a:[a_class [pagination; pagination_sm; mb0]]
      ( prev :: items @ [mobile_select; next] ))

let min_size, max_size = 5, 50
let size_choices = [ min_size; 10; 20; max_size ]

let make_page_sizer ?(page_sizer=true) ?(classes=[]) page page_size f =
  let onclick new_page_size () =
    let new_page = page * page_size / new_page_size in
    f new_page new_page_size in
  if not page_sizer then span ~a:[a_class ["hidden"]] []
  else
    let items =
      List.map (fun p -> make_dropdown_item (onclick p)
                   (txt @@ Printf.sprintf "%d" p))
        size_choices in
    let button =
      button ~a:[ a_button_type `Button; a_class ([btn; btn_sm] @ classes);
                  a_data_toggle "dropdown" ] [
        txt @@ Printf.sprintf "Rows: %d" page_size ] in
    make_dropdown button items

let make_refresh ?(refresh=true) ?(classes=[]) page page_size f =
  let onclick _ = f page page_size; false in
  if not refresh then span ~a:[a_class ["hidden"]] []
  else
    button ~a:[ a_button_type `Button; a_class ([btn; btn_sm] @ classes);
                a_onclick onclick] [
      span ~a:[ a_class ["fas"; "fa-redo"] ] [] ]

let rec make_pagination_elts
    ?classes ?refresh ?page_sizer id n seps current_page current_size f =
  let onclick page page_size =
    current_page := page;
    current_size := page_size;
    replace_pagination ?classes ?page_sizer id n seps current_page current_size f;
  in
  [
    make_page_sizer ?classes ?page_sizer !current_page !current_size onclick;
    span ~a:[ a_class [px1] ] [];
    make_paginate ?classes !current_page !current_size n seps onclick;
    span ~a:[ a_class [px1] ] [];
    make_refresh ?refresh ?classes !current_page !current_size onclick
  ]

and replace_pagination ?classes ?page_sizer id n seps current_page current_size f =
  let container = Js_utils.find_component id in
  let elts = make_pagination_elts ?classes ?page_sizer id n seps current_page current_size f in
  Js_utils.Manip.replaceChildren container elts;
  init_popovers ();
  List.iter (fun id -> fill_popover id [div [txt "test"]]) !seps;
  f !current_page !current_size

(* Table maker *)

type table_type =
  | Loading of string
  | Ready of (Html_types.tr Ocp_js.elt list * string * int)

let table_maker table_class theads = function
  | Loading table_id ->
    make_table ~responsive:[ btable_responsive ] ~table_class ~table_id
      theads []
  | Ready (rows, empty, ncolumns) ->
    if rows = [] then
      make_table ~table_class theads
        [ tr [ td ~a:[ a_colspan ncolumns ] [ txt empty ] ] ]
    else
      make_table ~table_class theads rows

let replace_opt def = function
  | None -> Some def
  | x -> x

(* Card Module Maker *)

module MakeCardTable(M: sig
    type t
    val name : string
    val title_span : int -> [> Html_types.span ] Ocp_js.elt
    val page_size : int
    val table_class : string list
    val card_classes : string list
    val heads : [> Html_types.th_content_fun ] Ocp_js.elt list
    val to_row : t -> [> Html_types.tr ] Ocp_js.elt
    val empty: string
  end) = struct

  let title_id = Printf.sprintf "%s-title" M.name
  let loading_id = Printf.sprintf "%s-loading" M.name
  let table_id = Printf.sprintf "%s-table" M.name
  let footer_id = Printf.sprintf "%s-footer" M.name

  let current_page = ref 0
  let current_size = ref M.page_size

  let theads = make_heads M.heads
  let ncolumns = List.length M.heads

  let make ?(footer=false) ?(suf_id="") ?(before=[]) ?(after=[])
      ?card_class ?card_id
      ?card_header_class ?card_header_id ?card_title_class
      ?card_body_class ?card_body_id
      ?card_footer_id ?card_footer_class
      () =
    let card_footer_content =
      if not footer then None
      else
        Some [ div ~a:[ a_class [d_flex] ] [
            div ~a:[ a_id (footer_id ^ suf_id) ; a_class [ mla ] ] []
          ]
          ] in
    let card_class = replace_opt M.card_classes card_class in
    let card_header_class = replace_opt M.card_classes card_header_class in
    let card_title_class = replace_opt M.card_classes card_title_class in
    let card_body_class = replace_opt M.card_classes card_body_class in
    let card_footer_class = replace_opt M.card_classes card_footer_class in
    make_card
      ?card_class ?card_id
      ?card_header_class ?card_header_id ?card_title_class
      ?card_body_class ?card_body_id
      ?card_footer_id ?card_footer_class
      ~card_title_content:(
        div [
          div ~a:[ a_class [ d_flex; justify_between ] ] [
            div ~a:[  a_id (title_id ^ suf_id) ] [
              M.title_span (-1) ];
            div ~a:[ a_class [d_inline_flex ]; a_id (loading_id ^ suf_id) ] [
              div ~a:[ a_class [ spinner_border; spinner_border_sm; mb3 ] ] []
            ]
          ]
        ])
      ~card_body_content:
        ([ div
             (before @ [ table_maker M.table_class theads
                           (Loading (table_id ^ suf_id))] @ after) ])
      ?card_footer_content ()

  let update_title ?title_elt ?(nrows=0) ?(suf_id="") () =
    let title_container = Js_utils.find_component (title_id ^ suf_id) in
    let title_elt = match title_elt with
      | None -> M.title_span nrows
      | Some title_elt -> title_elt in
    Js_utils.Manip.replaceChildren title_container [ title_elt ]

  let update_table ?(suf_id="") rows =
    let container = Js_utils.find_component (table_id ^ suf_id) in
    Js_utils.Manip.replaceChildren container
      [table_maker M.table_class theads (Ready (rows, M.empty, ncolumns))]

  let update ?(title=true) ?(suf_id="") ?page_sizer nrows xhr =
    let f page page_size =
      xhr page page_size (fun datas ->
          if title then update_title ~suf_id ~nrows ();
          update_table ~suf_id @@ List.map M.to_row datas) in
    replace_pagination ~classes:M.card_classes ?page_sizer
      (loading_id ^ suf_id) nrows (ref []) current_page current_size f

  let update_result ?(title=true) ?(suf_id="") ?page_sizer nrows xhr =
    let f page page_size : unit =
      xhr page page_size (function
          | Error msg -> Js_utils.log "%s" msg
          | Ok datas ->
            if title then update_title ~suf_id ~nrows ();
            update_table ~suf_id @@ List.map M.to_row datas) in
    replace_pagination ~classes:M.card_classes ?page_sizer
      (loading_id ^ suf_id) nrows (ref []) current_page current_size f

  let update_from_all ?(title=true) ?(suf_id="") ?page_sizer xhr =
    let nrows = ref 0 in
    let f page page_size =
      xhr (fun datas ->
          nrows := List.length datas;
          if title then update_title ~suf_id ~nrows:!nrows ();
          let pos = page * page_size in
          let next_pos = min (pos + page_size) !nrows in
          let rows = List.map M.to_row @@
            Array.to_list @@ Array.sub (Array.of_list datas) pos (next_pos-pos) in
          update_table ~suf_id rows) in
    replace_pagination ~classes:M.card_classes ?page_sizer
      (loading_id ^ suf_id) !nrows (ref []) current_page current_size f



end
