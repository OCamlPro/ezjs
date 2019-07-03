open Ocp_js
open Html
open Js_utils
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
      [> Html_types.div_content_fun ] Ocp_js.elt list * (* title *)
      [> Html_types.div_content_fun ] Ocp_js.elt list * (* body *)
      [> Html_types.div_content_fun ] Ocp_js.elt list option * (* footer *)
      Html_types.nmtoken list * (* header classes *)
      Html_types.nmtoken list * (* title classes *)
      Html_types.nmtoken list * (* body classes *)
      Html_types.nmtoken list * (* footer classes *)
      Html_types.nmtoken list (* buttons classes *)
  end) = struct

  let ht_info : (M.id, M.info) Hashtbl.t = Hashtbl.create 100
  let ht : (M.id, Html_types.div Ocp_js.elt) Hashtbl.t = Hashtbl.create 100
  let container : Html_types.div Ocp_js.elt ref = ref (div [])

  let make ?(card_class=[]) ?(card_header_class=[])
      ?(card_title_class=[]) ?(card_title_content=[]) () =
    container := div ~a:[ a_class card_class ] [];
    if card_title_content = [] then !container
    else
      div ~a:[ a_class [card; Border.border0] ] [
        div ~a:[a_class (card_header :: card_header_class)] [
          div ~a:[ a_class (card_title :: card_title_class) ] card_title_content ];
        div ~a:[ a_class [card_body; Border.border0]] [ !container ] ]

  let get_info id = Hashtbl.find_opt ht_info id

  let clear () =
    Hashtbl.clear ht;
    Hashtbl.clear ht_info;
    Js_utils.Manip.removeChildren !container

  let hide id = match Hashtbl.find_opt ht id with
    | None -> ()
    | Some elt -> Js_utils.hide elt

  let show id = match Hashtbl.find_opt ht id with
    | None -> ()
    | Some elt -> Js_utils.show elt

  let remove id =
    match Hashtbl.find_opt ht id with
    | None -> ()
    | Some elt ->
      Hashtbl.remove ht_info id;
      Hashtbl.remove ht id;
      Js_utils.Manip.removeChild !container elt

  let update_card card info =
    let _title, body, footer, header_class, _, body_class,
        footer_class, _button_class = M.make info in
    (match Js_utils.Manip.nth card 0 with
     | None -> ()
     | Some elt -> replace_classes elt (card_header :: header_class));
    (match Js_utils.Manip.nth card 1 with
     | None -> ()
     | Some elt -> Js_utils.Manip.replaceChildren elt body;
       replace_classes elt (card_body :: body_class));
    match footer, Js_utils.Manip.nth card 2 with
    | Some footer, Some elt -> Js_utils.Manip.replaceChildren elt footer;
      replace_classes elt (card_footer :: footer_class)
    | _ -> ()

  let update id info =
    match Hashtbl.find_opt ht id with
    | None -> ()
    | Some card -> update_card card info

  let append ?card_footer_id ?close ?minimize id info =
    let title, card_body_content, card_footer_content, card_header_class,
        card_title_class, card_body_class, card_footer_class, button_class =
      M.make info in
    let close_button = match close with
      | None -> []
      | Some close ->
        [ button ~a:[a_onclick (fun _ -> remove id; close id; true);
                     a_class (btn :: btn_sm :: Spacing.p0 :: button_class) ] [
            entity "times"] ] in
    let minimize_button = match minimize with
      | None -> []
      | Some minimize ->
        [ button ~a:[a_onclick (fun _ -> hide id; minimize id; true);
                     a_class (btn :: btn_sm :: Spacing.p0 :: button_class) ] [
            entity "minus"] ] in
    let header =
      div ~a:[ a_class [Display.d_flex; Flex.justify_between] ]
        (title @ minimize_button @ close_button ) in
    let card = make_card ~card_header_class ~card_title_class ~card_body_class
        ~card_title_content:header ~card_body_content ?card_footer_content
        ?card_footer_id
        ~card_footer_class () in
    Hashtbl.add ht_info id info;
    Hashtbl.add ht id card;
    Js_utils.Manip.appendChild !container card

  let update_or_append ?card_footer_id ?close ?minimize id info =
    match Hashtbl.find_opt ht id with
    | None -> append ?card_footer_id ?close ?minimize id info
    | Some card -> update_card card info

  let append_all l = List.iter (fun (id, info) -> append id info) l
  let make_all l = append_all l; !container


end
