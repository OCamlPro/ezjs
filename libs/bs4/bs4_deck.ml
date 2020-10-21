open Ezjs_tyxml
open Html
open Bs4.Items.BCard
open Bs4.Items.Button
open Bs4.Utils

let replace_classes elt classes =
  let has_show = Manip.containsClass elt "show" in
  let classes = if has_show then "show" :: classes else classes in
  Manip.removeAttribute elt "class";
  Manip.setAttribute elt "class" (String.concat " " classes)

module Make(M: sig
    type info
    type id
    val make : info ->
      [> Html_types.div_content_fun ] elt list * (* title *)
      [> Html_types.div_content_fun ] elt list * (* body *)
      [> Html_types.div_content_fun ] elt list option * (* footer *)
      Html_types.nmtoken list * (* header classes *)
      Html_types.nmtoken list * (* title classes *)
      Html_types.nmtoken list * (* body classes *)
      Html_types.nmtoken list * (* footer classes *)
      Html_types.nmtoken list (* buttons classes *)
    val hide_main : string option
    val buttons : (info -> unit) option * (info -> unit) option (* hide, close *)
    val footer_id : (id -> string) option
  end) = struct

  let ht_info : (M.id, M.info) Hashtbl.t = Hashtbl.create 100
  let ht : (M.id, Html_types.div elt) Hashtbl.t = Hashtbl.create 100
  let container : Html_types.div elt ref = ref (div [])
  let hidden = ref (M.hide_main <> None)

  let make ?(card_class=[]) ?(card_header_class=[])
      ?(card_title_class=[]) ?(card_title_content=[]) ?(card_body_class=[]) () =
    container := div ~a:[ a_class card_class ] [];
    if card_title_content = [] then !container
    else
      let attrs = match M.hide_main with
        | None -> []
        | Some id -> [ a_style "display: none"; a_id id ] in
      div ~a:((a_class [card; Border.border0]) :: attrs) [
        div ~a:[a_class (card_header :: card_header_class)] [
          div ~a:[ a_class (card_title :: card_title_class) ] card_title_content ];
        div ~a:[ a_class (card_body :: Border.border0 :: card_body_class)] [ !container ] ]

  let get_info id = Hashtbl.find_opt ht_info id

  let hide_main () = match M.hide_main with
    | None -> ()
    | Some id -> hide (find_component id)
  let show_main () = match M.hide_main with
    | None -> ()
    | Some id -> show (find_component id)

  let clear () =
    Hashtbl.clear ht;
    Hashtbl.clear ht_info;
    Manip.removeChildren !container;
    match M.hide_main with
    | None -> ()
    | Some id -> hidden := true; hide (find_component id)

  let hide id = match Hashtbl.find_opt ht id with
    | None -> ()
    | Some elt -> hide elt

  let show id = match Hashtbl.find_opt ht id with
    | None -> ()
    | Some elt -> show elt

  let remove id =
    match Hashtbl.find_opt ht id with
    | None -> ()
    | Some elt ->
      Hashtbl.remove ht_info id;
      Hashtbl.remove ht id;
      Manip.removeChild !container elt;
      match M.hide_main, Manip.childLength !container with
      | Some id, 0 -> hidden := true; Ezjs_tyxml.hide (find_component id)
      | _ -> ()

  let make_card id info =
    let title, card_body_content, card_footer_content, card_header_class,
        card_title_class, card_body_class, card_footer_class, button_class =
      M.make info in
    let minimize_button = match fst M.buttons with
      | None -> []
      | Some minimize ->
        [ button ~a:[a_onclick (fun _ -> hide id; minimize info; true);
                     a_class ([btn; btn_sm; Spacing.py0] @ button_class) ] [
            entity "minus"] ] in
    let close_button = match snd M.buttons with
      | None -> []
      | Some close ->
        [ button ~a:[a_onclick (fun _ -> remove id; close info; true);
                     a_class ([btn; btn_sm; Spacing.py0] @ button_class) ] [
            entity "times"] ] in
    let buttons = match minimize_button, close_button with
      | [], [] -> []
      | _ -> [ div (minimize_button @ close_button) ] in
    let header =
      div ~a:[ a_class [Display.d_flex; Flex.justify_between] ]
        (title @  buttons) in
    let card_footer_id = match M.footer_id with None -> None | Some f -> Some (f id) in
    make_card ~card_header_class ~card_title_class ~card_body_class
      ~card_title_content:header ~card_body_content ?card_footer_content
      ?card_footer_id
      ~card_footer_class ()

  let update_card card id info =
    let children = Manip.children @@ make_card id info in
    Manip.replaceChildren card children

  let append id info =
    let card = make_card id info in
    Hashtbl.add ht_info id info;
    Hashtbl.add ht id card;
    Manip.appendChild !container card;
    if !hidden then
      match M.hide_main with
      | Some id -> hidden := false; Ezjs_tyxml.show (find_component id)
      | _ -> ()

  let update id info =
    match Hashtbl.find_opt ht id with
    | None -> ()
    | Some card -> update_card card id info

  let update_or_append id info =
    match Hashtbl.find_opt ht id with
    | None -> append id info
    | Some card -> update_card card id info

  let append_all l = List.iter (fun (id, info) -> append id info) l
  let make_all l = append_all l; !container


end
