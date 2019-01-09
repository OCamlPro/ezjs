open Ocp_js.Html

module Attribute = struct
  let a_aria attr value =
    to_attrib @@ Ocp_js.Xml.string_attrib ("aria-" ^ attr) value

  let a_role value =
    to_attrib @@ Ocp_js.Xml.string_attrib "role" value

  let a_data_custom key value =
    to_attrib @@ Ocp_js.Xml.string_attrib
      (Printf.sprintf "data-%s" key) value

  let a_data_toggle value =
    a_data_custom "toggle" value

  let a_data_placement value =
    let value =
      match value with
      | `Top -> "top" | `Left -> "left" | `Right -> "right" | `Bottom -> "bottom"
    in
    a_data_custom "placement" value

  let a_data_content value =
    a_data_custom "content" value

  let a_data_html value =
    a_data_custom "html" (string_of_bool value)

  let a_data_template value =
    a_data_custom "html" value

  let a_data_trigger value =
    a_data_custom "trigger" value

  let a_attrib key value =
    to_attrib @@ Ocp_js.Xml.string_attrib key value
end

module Misc = struct
  let active = "active"
  let disabled = "disabled"
  let clearfix = "clearfix"
  let close = "close"
  let aria_close = Attribute.a_aria "label" "Close"
  let text_hide = "text-hide"
  let sr_only = "sr-only"
  let sr_focusable = "sr-only-focusable"
  let visible = "visible"
  let invisible = "invisible"
end

module Icon = struct
  let glyph name = span ~a:[ a_class ["glyphicon"; "glyphicon-" ^ name ] ] []
  let glyph_u name () = glyph name
  let fas name = span ~a:[ a_class ["fas"; "fa-" ^ name ] ] []
  let fas_u name () = fas name
  let number_icon icon =  span [pcdata "# "; icon ]
  let number_icon_u icon () =  number_icon icon
end

module Grid = struct
  let container = "container"
  let container_fluid = "container-fluid"
  let row = "row"

  let xs_size = 576
  let sm_size = 768
  let md_size = 992
  let lg_size = 1200

  let col1 = "col-1"
  let col2 = "col-2"
  let col3 = "col-3"
  let col4 = "col-4"
  let col5 = "col-5"
  let col6 = "col-6"
  let col7 = "col-7"
  let col8 = "col-8"
  let col9 = "col-9"
  let col10 = "col-10"
  let col11 = "col-11"
  let col12 = "col-12"
  let colauto = "col-auto"

  let csm1 = "col-sm-1"
  let csm2 = "col-sm-2"
  let csm3 = "col-sm-3"
  let csm4 = "col-sm-4"
  let csm5 = "col-sm-5"
  let csm6 = "col-sm-6"
  let csm7 = "col-sm-7"
  let csm8 = "col-sm-8"
  let csm9 = "col-sm-9"
  let csm10 = "col-sm-10"
  let csm11 = "col-sm-11"
  let csm12 = "col-sm-12"
  let csmauto = "col-sm-auto"

  let cmd1 = "col-md-1"
  let cmd2 = "col-md-2"
  let cmd3 = "col-md-3"
  let cmd4 = "col-md-4"
  let cmd5 = "col-md-5"
  let cmd6 = "col-md-6"
  let cmd7 = "col-md-7"
  let cmd8 = "col-md-8"
  let cmd9 = "col-md-9"
  let cmd10 = "col-md-10"
  let cmd11 = "col-md-11"
  let cmd12 = "col-md-12"
  let cmdauto = "col-md-auto"

  let clg1 = "col-lg-1"
  let clg2 = "col-lg-2"
  let clg3 = "col-lg-3"
  let clg4 = "col-lg-4"
  let clg5 = "col-lg-5"
  let clg6 = "col-lg-6"
  let clg7 = "col-lg-7"
  let clg8 = "col-lg-8"
  let clg9 = "col-lg-9"
  let clg10 = "col-lg-10"
  let clg11 = "col-lg-11"
  let clg12 = "col-lg-12"
  let clgauto = "col-lg-auto"

  let cxl1 = "col-xl-1"
  let cxl2 = "col-xl-2"
  let cxl3 = "col-xl-3"
  let cxl4 = "col-xl-4"
  let cxl5 = "col-xl-5"
  let cxl6 = "col-xl-6"
  let cxl7 = "col-xl-7"
  let cxl8 = "col-xl-8"
  let cxl9 = "col-xl-9"
  let cxl10 = "col-xl-10"
  let cxl11 = "col-xl-11"
  let cxl12 = "col-xl-12"
  let cxlauto = "col-xl-auto"

  let off0 = "offset-0"
  let off1 = "offset-1"
  let off2 = "offset-2"
  let off3 = "offset-3"
  let off4 = "offset-4"
  let off5 = "offset-5"
  let off6 = "offset-6"
  let off7 = "offset-7"
  let off8 = "offset-8"
  let off9 = "offset-9"
  let off10 = "offset-10"
  let off11 = "offset-11"
  let off12 = "offset-12"

  let offsm0 = "offset-sm-0"
  let offsm1 = "offset-sm-1"
  let offsm2 = "offset-sm-2"
  let offsm3 = "offset-sm-3"
  let offsm4 = "offset-sm-4"
  let offsm5 = "offset-sm-5"
  let offsm6 = "offset-sm-6"
  let offsm7 = "offset-sm-7"
  let offsm8 = "offset-sm-8"
  let offsm9 = "offset-sm-9"
  let offsm10 = "offset-sm-10"
  let offsm11 = "offset-sm-11"
  let offsm12 = "offset-sm-12"

  let offmd0 = "offset-md-0"
  let offmd1 = "offset-md-1"
  let offmd2 = "offset-md-2"
  let offmd3 = "offset-md-3"
  let offmd4 = "offset-md-4"
  let offmd5 = "offset-md-5"
  let offmd6 = "offset-md-6"
  let offmd7 = "offset-md-7"
  let offmd8 = "offset-md-8"
  let offmd9 = "offset-md-9"
  let offmd10 = "offset-md-10"
  let offmd11 = "offset-md-11"
  let offmd12 = "offset-md-12"

  let offlg0 = "offset-lg-0"
  let offlg1 = "offset-lg-1"
  let offlg2 = "offset-lg-2"
  let offlg3 = "offset-lg-3"
  let offlg4 = "offset-lg-4"
  let offlg5 = "offset-lg-5"
  let offlg6 = "offset-lg-6"
  let offlg7 = "offset-lg-7"
  let offlg8 = "offset-lg-8"
  let offlg9 = "offset-lg-9"
  let offlg10 = "offset-lg-10"
  let offlg11 = "offset-lg-11"
  let offlg12 = "offset-lg-12"

  let offxl0 = "offset-xl-0"
  let offxl1 = "offset-xl-1"
  let offxl2 = "offset-xl-2"
  let offxl3 = "offset-xl-3"
  let offxl4 = "offset-xl-4"
  let offxl5 = "offset-xl-5"
  let offxl6 = "offset-xl-6"
  let offxl7 = "offset-xl-7"
  let offxl8 = "offset-xl-8"
  let offxl9 = "offset-xl-9"
  let offxl10 = "offset-xl-10"
  let offxl11 = "offset-xl-11"
  let offxl12 = "offset-xl-12"

  let ord1 = "order-1"
  let ord2 = "order-2"
  let ord3 = "order-3"
  let ord4 = "order-4"
  let ord5 = "order-5"
  let ord6 = "order-6"
  let ord7 = "order-7"
  let ord8 = "order-8"
  let ord9 = "order-9"
  let ord10 = "order-10"
  let ord11 = "order-11"
  let ord12 = "order-12"

  let ord1_sm = "order-sm-1"
  let ord2_sm = "order-sm-2"
  let ord3_sm = "order-sm-3"
  let ord4_sm = "order-sm-4"
  let ord5_sm = "order-sm-5"
  let ord6_sm = "order-sm-6"
  let ord7_sm = "order-sm-7"
  let ord8_sm = "order-sm-8"
  let ord9_sm = "order-sm-9"
  let ord10_sm = "order-sm-10"
  let ord11_sm = "order-sm-11"
  let ord12_sm = "order-sm-12"

  let ord1_md = "order-md-1"
  let ord2_md = "order-md-2"
  let ord3_md = "order-md-3"
  let ord4_md = "order-md-4"
  let ord5_md = "order-md-5"
  let ord6_md = "order-md-6"
  let ord7_md = "order-md-7"
  let ord8_md = "order-md-8"
  let ord9_md = "order-md-9"
  let ord10_md = "order-md-10"
  let ord11_md = "order-md-11"
  let ord12_md = "order-md-12"

  let ord1_lg = "order-lg-1"
  let ord2_lg = "order-lg-2"
  let ord3_lg = "order-lg-3"
  let ord4_lg = "order-lg-4"
  let ord5_lg = "order-lg-5"
  let ord6_lg = "order-lg-6"
  let ord7_lg = "order-lg-7"
  let ord8_lg = "order-lg-8"
  let ord9_lg = "order-lg-9"
  let ord10_lg = "order-lg-10"
  let ord11_lg = "order-lg-11"
  let ord12_lg = "order-lg-12"

  let ord1_xl = "order-xl-1"
  let ord2_xl = "order-xl-2"
  let ord3_xl = "order-xl-3"
  let ord4_xl = "order-xl-4"
  let ord5_xl = "order-xl-5"
  let ord6_xl = "order-xl-6"
  let ord7_xl = "order-xl-7"
  let ord8_xl = "order-xl-8"
  let ord9_xl = "order-xl-9"
  let ord10_xl = "order-xl-10"
  let ord11_xl = "order-xl-11"
  let ord12_xl = "order-xl-12"

  let mrxs = "mr-auto"
  let mrsm = "mr-sm-auto"
  let mrmd = "mr-md-auto"
  let mrlg = "mr-lg-auto"
  let mrxl = "mr-xl-auto"
  let mlxs = "ml-auto"
  let mlsm = "ml-sm-auto"
  let mlmd = "ml-md-auto"
  let mllg = "ml-lg-auto"
  let mlxl = "ml-xl-auto"
end

module Text = struct
  let bold = "font-weight-bold"
  let italic = "font-italic"
  let light = "font-weight-light"
  let normal = "font-weight-normal"
  let lead = "lead"
  let small = "small"
  let leftxs = "text-left"
  let leftsm = "text-sm-left"
  let leftmd = "text-md-left"
  let leftlg = "text-lg-left"
  let leftxl = "text-xl-left"
  let rightxs = "text-right"
  let rightsm = "text-sm-right"
  let rightmd = "text-md-right"
  let rightlg = "text-lg-right"
  let rightxl = "text-xl-right"
  let justify = "text-justify"
  let monospace = "text-monospace"
  let nowrap = "text-nowrap"
  let truncate = "text-truncate"
  let lowercase = "text-lowercase"
  let uppercase = "text-uppercase"
  let capitalize = "text-capitalize"
  let initialism = "initialism"
  let list_unstyled = "list-unstyled"
  let list_inline = "list-inline"
  let list_inline_item = "list-inline-item"
  let pre_scrollable = "pre-scrollable"
  let jumbotron = "jumbotron"
end

module Color = struct
  let muted_tx = "text-muted"
  let primary_tx = "text-primary"
  let success_tx = "text-success"
  let info_tx = "text-info"
  let warning_tx = "text-warning"
  let danger_tx = "text-danger"
  let secondary_tx = "text-secondary"
  let white_tx = "text-white"
  let dark_tx = "text-dark"
  let light_tx = "text-light"
  let body_tx = "text-body"
  let black50_tx = "text-black-50"
  let white50_tx = "text-white-50"
  let primary_bg = "bg-primary"
  let success_bg = "bg-success"
  let info_bg = "bg-info"
  let warning_bg = "bg-warning"
  let danger_bg = "bg-danger"
  let secondary_bg = "bg-secondary"
  let white_bg = "bg-white"
  let dark_bg = "bg-dark"
  let light_bg = "bg-light"
end

module Table = struct
  let btable = "table"
  let btable_striped = "table-striped"
  let btable_bordered = "table-bordered"
  let btable_hover = "table-hover"
  let btable_dark = "table-dark"
  let btale_borderless = "table-borderless"
  let bt_primaty = "table-primary"
  let bt_success = "table-success"
  let bt_danger = "table-danger"
  let bt_info = "table-info"
  let bt_warning = "table-warning"
  let bt_active = "table-active"
  let bt_secondary = "table-secondary"
  let bt_light = "table-light"
  let bt_dark = "table-dark"
  let thead_dark = "thead-dark"
  let thead_light = "thead-light"
  let btable_small = "table-sm"
  let btable_responsive = "table-responsive"
  let btable_responsive_sm = "table-responsive-sm"
  let btable_responsive_md = "table-responsive-md"
  let btable_responsive_lg = "table-responsive-lg"
  let btable_responsive_xl = "table-responsive-xl"

  let make_heads ?(head_class=[]) l =
    thead ~a:[ a_class head_class] [
      tr (List.map (fun elt -> th ~a:[ a_scope `Col ] [ elt ]) l) ]

  let make_table ?(responsive=[]) ?(table_class=[]) heads ltr =
    let t =
      tablex ~a:[ a_class (btable :: table_class)] [
        heads;
        tbody ltr ] in
    if responsive = [] then t else
      div ~a:[ a_class responsive ] [ t ]
end

module Image = struct
  let rounded = "rounded"
  let circle = "rounded-circle"
  let thumbnail = "img-thumbnail"
  let fluid = "img-fluid"
  let centered = "mx-auto d-block"
end

module Alert = struct
  let alert = "alert"
  let asuccess = "alert-success"
  let ainfo = "alert-info"
  let awarning = "alert-warning"
  let adanger = "alert-danger"
  let aprimary = "alert-primary"
  let asecondary = "alert-secondary"
  let adark = "alert-dark"
  let alight = "alert-light"
  let alert_link = "alert-link"
  let alert_dismissible = "alert-dismissible"
  let afade = "fade"
  let ashow = "show"
end

module Badge = struct
  let badge = "badge"
  let badge_primary = "badge-primary"
  let badge_secondary = "badge-secondary"
  let badge_success = "badge-success"
  let badge_info = "badge-info"
  let badge_warning = "badge-warning"
  let badge_danger = "badge-danger"
  let badge_dark = "badge-dark"
  let badge_light = "badge-light"
  let badge_pill = "badge-pill"
end

module Breadcrumb = struct
  let breadcrumb = "breadcrumb"
  let breadcrumb_item = "breadcrumb-item"
  let make_breadcrumb l =
    let rec f = function
      | [] -> []
      | [ name, _url ] ->
        [ li ~a:[ a_class [breadcrumb_item; Misc.active];
                  Attribute.a_aria "current" "page"] [ txt name ] ]
      | (name, url) :: t ->
        (li ~a:[ a_class [breadcrumb_item] ] [
            a ~a:[ a_href url ] [ txt name ] ]) :: (f t) in
    nav ~a:[ Attribute.a_aria "label" breadcrumb ] [
      ol ~a:[ a_class [breadcrumb] ] (f l)
    ]

end

module Button = struct
  let btn = "btn"
  let btn_primary = "btn-primary"
  let btn_secondary = "btn-secondary"
  let btn_success = "btn-success"
  let btn_info = "btn-info"
  let btn_warning = "btn-warning"
  let btn_danger = "btn-danger"
  let btn_dark = "btn-dark"
  let btn_light = "btn-light"
  let btn_link = "btn-link"
  let btn_sm = "btn-sm"
  let btn_lg = "btn-lg"
  let btn_block = "btn-block"
  let btn_group = "btn-group"
  let btn_group_sm = "btn-group-sm"
  let btn_group_lg = "btn-group-lg"
  let btn_group_vertical = "btn-group-vertical"
end

module Card = struct
  let card = "card"
  let card_header = "card-header"
  let card_body = "card-body"
  let card_footer = "card-footer"
  let card_text = "card-text"
  let card_link = "card-link"
  let card_title = "card-title"
  let card_subtitle = "card-subtitle"
  let card_tabs = "card-header-tabs"
  let card_pills = "card-header-pills"
  let card_img_top = "card-img-top"
  let card_img_bottom = "card-img-bottom"
  let card_group = "card-group"
  let card_deck = "card-deck"
  let card_colums = "card-columns"
  let card_primary = "card-primary"
  let card_secondary = "card-secondary"
  let card_success = "card-success"
  let card_info = "card-info"
  let card_warning = "card-warning"
  let card_danger = "card-danger"
  let card_dark = "card-dark"
  let card_light = "card-light"

  let make_card
      ?(card_class=[]) ?card_id (* card info *)
      ?(card_header_class=[]) ?card_header_id (* card heading info *)
      ?(card_title_class=[]) ?(card_title_content) ?(card_title_extra=[]) (* card title info *)
      ?(card_body_class=[]) ?card_body_id ?(card_body_content=[]) (* card body info *)
      ?(card_footer_class=[]) ?card_footer_id ?card_footer_content (* card footer info *)
      () =
    let unopt_id = function None -> [] | Some id -> [ a_id id ] in
    let card_id, card_header_id, card_body_id, card_footer_id =
      unopt_id card_id, unopt_id card_header_id,
      unopt_id card_body_id, unopt_id card_footer_id in
    let card_title_content = match card_title_content with
      | None -> []
      | Some elt ->
        List.iter (Js_utils.Manip.addClass elt) (card_title :: card_title_class) ;
        [ elt ] in
    let div_title = if card_title_content = [] then [] else
        [ div ~a:(a_class (card_header :: card_header_class) :: card_header_id) (
              card_title_content @ card_title_extra) ] in
    let div_footer = match card_footer_content with
      | None -> []
      | Some footer_content ->
        [ div ~a:(a_class (card_footer :: card_footer_class) :: card_footer_id)
            footer_content ] in
    let div_body =
      div ~a:(a_class (card_body :: card_body_class) :: card_body_id)
        card_body_content in
    div ~a:(a_class (card :: card_primary :: card_class) :: card_id) (
      div_title @ [ div_body ] @ div_footer )
end

module Carousel = struct
  let carousel = "carousel slide"
  let carousel_caption = "carousel-caption"
  let carousel_next = "carousel-control-next"
  let carousel_next_icon = "carousel-control-next-icon"
  let carousel_prev = "carousel-control-prev"
  let carousel_prev_icon = "carousel-control-prev-icon"
  let carousel_indicators = "carousel-indicators"
  let carousel_inner = "carousel-inner"
  let carousel_item = "carousel-item"

  let make_carousel
      ?(carousel_class=[]) ?carousel_id ?(img_class=[])
      ?(controls=false) ?(indicators=false)
      ?(captions=[]) l =
    let controls, indicators =
      if carousel_id = None then false, false else controls, indicators in
    let captions =
      if List.length captions <> List.length l then [] else
        List.map (fun (title, subtitle) ->
            div ~a:[ a_class [carousel_caption; "d-none"; "d-md-block"] ] [
              h5 [ txt title ];
              p [ txt subtitle ] ]) captions in
    let carousel_id, carousel_link = match carousel_id with
      | None -> [], "#"
      | Some id -> [ a_id id ], "#" ^ id in
    let controls =
      if controls then [
        a ~a:[ a_class [carousel_prev]; a_href carousel_link; a_role ["button"];
               Attribute.a_data_custom "slide" "prev"] [
          span ~a:[ a_class [carousel_prev_icon]; Attribute.a_aria "hidden" "true" ] [];
          span ~a:[ a_class ["sr-only"] ] [ txt "Previous" ] ];
        a ~a:[ a_class [carousel_next]; a_href carousel_link; a_role ["button"];
               Attribute.a_data_custom "slide" "next"] [
          span ~a:[ a_class [carousel_next_icon]; Attribute.a_aria "hidden" "true" ] [];
          span ~a:[ a_class ["sr-only"] ] [ txt "Next" ] ] ]
      else [] in
    let indicators =
      if indicators then [
        ol ~a:[ a_class [carousel_indicators] ] (
          List.mapi (fun i _ ->
              let first_class = if i = 0 then [ Misc.active ] else [] in
              li ~a:[ Attribute.a_data_custom "target" carousel_link;
                      Attribute.a_data_custom "slide-to" (string_of_int i);
                      a_class first_class ] []) l) ]
      else [] in
    div ~a:( carousel_id @ [ a_class (carousel :: carousel_class @ ["d-block"; "w-100"]);
                              Attribute.a_data_custom "ride" carousel ]) (
      indicators @ [
        div ~a:[ a_class [carousel_inner] ] (
          List.mapi (fun i (src, alt) ->
              let first_class = if i = 0 then [ Misc.active ] else [] in
              let caption = if captions = [] then [] else [List.nth captions i] in
              div ~a:[ a_class (carousel_item :: first_class) ] (
                (img ~a:[ a_class img_class ] ~src ~alt ()) ::
                caption) ) l) ] @
      controls )
end

module Collapse = struct
  let collapse = "collapse"
  let collapsing = "collapsing"
  let accordion = "accordion"
  let toggle = Attribute.a_data_custom "toggle" "collapse"
end

module Dropdown = struct
  let dropdown = "dropdown"
  let dropdown_toggle = "dropdown-toggle"
  let dropdown_menu = "dropdown-menu"
  let dropdown_item = "dropdown-item"
  let dropdown_split = "dropdown-toggle-split"
  let dropdown_divider = "dropdown-divider"
  let dropdown_header = "dropdown-header"
  let dropdown_item_text = "dropdown-item-text"
  let dropdown_menu_right = "dropdown-menu-right"
  let dropleft = "dropleft"
  let dropright = "dropright"
  let dropup = "dropup"

  let make_dropdown button button_id items =
    div ~a:[ a_class [dropdown] ] [
      button;
      div ~a:[ a_class [dropdown_menu]; Attribute.a_aria "labelledby" button_id] items ]
end

module Form = struct
  let form_check = "form-check"
  let form_check_inline = "form-check-inline"
  let form_check_input = "form-check-input"
  let form_check_label = "form-check-label"
  let form_control = "form-control"
  let form_control_file = "form-control-file"
  let form_control_lg = "form-control-lg"
  let form_control_plaintext = "form-control-plaintext"
  let form_control_range = "form-control-range"
  let form_control_sm = "form-control-sm"
  let form_group = "form-group"
  let form_inline = "form-inline"
end

module Modal = struct
  let modal = "modal"
  let modal_body = "modal-body"
  let modal_content = "modal-content"
  let modal_dialog = "modal-dialog-centered"
  let modal_header = "modal-header"
  let modal_footer = "modal-footer"
  let modal_lg = "modal-lg"
  let modal_sm = "modal-sm"
end

module Nav = struct
  let nav = "nav"
  let nav_item = "nav-item"
  let nav_link = "nav-link"
  let nav_tabs = "nav-tabs"
  let nav_pills = "nav-pills"
  let nav_justified = "nav-justified"
  let navbar = "navbar"
  let navbar_nav = "navbar-nav"
  let navbar_brand = "navbar-brand"
  let navbar_collapse = "navbar-collapse"
  let navbar_expand_sm = "navbar-expand-sm"
  let navbar_expand_md = "navbar-expand-md"
  let navbar_expand_lg = "navbar-expand-lg"
  let navbar_expand_xl = "navbar-expand-xl"
  let navbar_dark = "navbar-dark"
  let navbar_light = "navbar-light"
  let navbar_text = "navbar-text"
  let navbar_toggler = "navbar-toggler"
end

module Pagination = struct
  let pagination = "pagination"
  let pagination_sm = "pagination-sm"
  let pagination_lg = "pagination-lg"
  let page_item = "page-item"
  let page_link = "page-link"
end

module Progress = struct
  let progress = "progress"
  let progress_bar = "progress-bar"
  let progress_bar_animated = "progress-bar-animated"
  let progress_bar_striped = "progress-bar-striped"
  let progress_role = Attribute.a_role "progressbar"
end

module Border = struct
  let border = "border"
  let border_top = "border-top"
  let border_bottom = "border-bottom"
  let border_left = "border-left"
  let border_right = "border-right"
  let border0 = "border-0"
  let border_top0 = "border-top-0"
  let border_bottom0 = "border-bottom-0"
  let border_left0 = "border-left-0"
  let border_right0 = "border-right-0"
  let border_primary = "border-primary"
  let border_secondary = "border-secondary"
  let border_success = "border-success"
  let border_info = "border-info"
  let border_warning = "border-warning"
  let border_danger = "border-danger"
  let border_dark = "border-dark"
  let border_light = "border-light"
  let border_white = "border-white"
  let rounded = "rounded"
  let rounded_top = "rounded-top"
  let rounded_right = "rounded-right"
  let rounded_left = "rounded-left"
  let rounded_circle = "rounded-circle"
  let rounded0 = "rounded-0"
end

module Display = struct
  let d_none = "d-none"
  let d_none_sm = "d-sm-none"
  let d_none_md = "d-md-none"
  let d_none_lg = "d-lg-none"
  let d_none_xl = "d-xl-none"
  let d_inline = "d-inline"
  let d_inline_sm = "d-sm-inline"
  let d_inline_md = "d-md-inline"
  let d_inline_lg = "d-lg-inline"
  let d_inline_xl = "d-xl-inline"
  let d_inline_block = "d-inline-block"
  let d_inline_block_sm = "d-sm-inline-block"
  let d_inline_block_md = "d-md-inline-block"
  let d_inline_block_lg = "d-lg-inline-block"
  let d_inline_block_xl = "d-xl-inline-block"
  let d_block = "d-block"
  let d_block_sm = "d-sm-block"
  let d_block_md = "d-md-block"
  let d_block_lg = "d-lg-block"
  let d_block_xl = "d-xl-block"
  let d_table = "d-table"
  let d_table_sm = "d-sm-table"
  let d_table_md = "d-md-table"
  let d_table_lg = "d-lg-table"
  let d_table_xl = "d-xl-table"
  let d_table_cell = "d-table-cell"
  let d_table_cell_sm = "d-sm-table-cell"
  let d_table_cell_md = "d-md-table-cell"
  let d_table_cell_lg = "d-lg-table-cell"
  let d_table_cell_xl = "d-xl-table-cell"
  let d_table_row = "d-table-row"
  let d_table_row_sm = "d-sm-table-row"
  let d_table_row_md = "d-md-table-row"
  let d_table_row_lg = "d-lg-table-row"
  let d_table_row_xl = "d-xl-table-row"
  let d_flex = "d-flex"
  let d_flex_sm = "d-sm-flex"
  let d_flex_md = "d-md-flex"
  let d_flex_lg = "d-lg-flex"
  let d_flex_xl = "d-xl-flex"
  let d_inline_flex = "d-inline-flex"
  let d_inline_flex_sm = "d-sm-inline-flex"
  let d_inline_flex_md = "d-md-inline-flex"
  let d_inline_flex_lg = "d-lg-inline-flex"
  let d_inline_flex_xl = "d-xl-inline-flex"
end

module Flex = struct
  let flex_column = "flex-column"
  let flex_column_rev = "flex-column-reverse"
  let flex_row = "flex-row"
  let flex_row_rev = "flex-row-reverse"
  let flex_column_sm = "flex-sm-column"
  let flex_column_rev_sm = "flex-sm-column-reverse"
  let flex_row_sm = "flex-sm-row"
  let flex_row_rev_sm = "flex-sm-row-reverse"
  let flex_column_md = "flex-md-column"
  let flex_column_rev_md = "flex-md-column-reverse"
  let flex_row_md = "flex-md-row"
  let flex_row_rev_md = "flex-md-row-reverse"
  let flex_column_lg = "flex-lg-column"
  let flex_column_rev_lg = "flex-lg-column-reverse"
  let flex_row_lg = "flex-lg-row"
  let flex_row_rev_lg = "flex-lg-row-reverse"
  let flex_column_xl = "flex-xl-column"
  let flex_column_rev_xl = "flex-xl-column-reverse"
  let flex_row_xl = "flex-xl-row"
  let flex_row_rev_xl = "flex-xl-row-reverse"

  let justify_start = "justify-content-start"
  let justify_end = "justify-content-end"
  let justify_center = "justify-content-center"
  let justify_between = "justify-content-between"
  let justify_around = "justify-content-around"
  let justify_start_sm = "justify-content-sm-start"
  let justify_end_sm = "justify-content-sm-end"
  let justify_center_sm = "justify-content-sm-center"
  let justify_between_sm = "justify-content-sm-between"
  let justify_around_sm = "justify-content-sm-around"
  let justify_start_md = "justify-content-md-start"
  let justify_end_md = "justify-content-md-end"
  let justify_center_md = "justify-content-md-center"
  let justify_between_md = "justify-content-md-between"
  let justify_around_md = "justify-content-md-around"
  let justify_start_lg = "justify-content-lg-start"
  let justify_end_lg = "justify-content-lg-end"
  let justify_center_lg = "justify-content-lg-center"
  let justify_between_lg = "justify-content-lg-between"
  let justify_around_lg = "justify-content-lg-around"
  let justify_start_xl = "justify-content-xl-start"
  let justify_end_xl = "justify-content-xl-end"
  let justify_center_xl = "justify-content-xl-center"
  let justify_between_xl = "justify-content-xl-between"
  let justify_around_xl = "justify-content-xl-around"

  let align_items_start = "align-items-start"
  let align_items_end = "align-items-end"
  let align_items_center = "align-items-center"
  let align_items_baseline = "align-items-baseline"
  let align_items_stretch = "align-items-stretch"
  let align_items_start_sm = "align-items-sm-start"
  let align_items_end_sm = "align-items-sm-end"
  let align_items_center_sm = "align-items-sm-center"
  let align_items_baseline_sm = "align-items-sm-baseline"
  let align_items_stretch_sm = "align-items-sm-stretch"
  let align_items_start_md = "align-items-md-start"
  let align_items_end_md = "align-items-md-end"
  let align_items_center_md = "align-items-md-center"
  let align_items_baseline_md = "align-items-md-baseline"
  let align_items_stretch_md = "align-items-md-stretch"
  let align_items_start_lg = "align-items-lg-start"
  let align_items_end_lg = "align-items-lg-end"
  let align_items_center_lg = "align-items-lg-center"
  let align_items_baseline_lg = "align-items-lg-baseline"
  let align_items_stretch_lg = "align-items-lg-stretch"
  let align_items_start_xl = "align-items-xl-start"
  let align_items_end_xl = "align-items-xl-end"
  let align_items_center_xl = "align-items-xl-center"
  let align_items_baseline_xl = "align-items-xl-baseline"
  let align_items_stretch_xl = "align-items-xl-stretch"

  let align_self_start = "align-self-start"
  let align_self_end = "align-self-end"
  let align_self_center = "align-self-center"
  let align_self_baseline = "align-self-baseline"
  let align_self_stretch = "align-self-stretch"
  let align_self_start_sm = "align-self-sm-start"
  let align_self_end_sm = "align-self-sm-end"
  let align_self_center_sm = "align-self-sm-center"
  let align_self_baseline_sm = "align-self-sm-baseline"
  let align_self_stretch_sm = "align-self-sm-stretch"
  let align_self_start_md = "align-self-md-start"
  let align_self_end_md = "align-self-md-end"
  let align_self_center_md = "align-self-md-center"
  let align_self_baseline_md = "align-self-md-baseline"
  let align_self_stretch_md = "align-self-md-stretch"
  let align_self_start_lg = "align-self-lg-start"
  let align_self_end_lg = "align-self-lg-end"
  let align_self_center_lg = "align-self-lg-center"
  let align_self_baseline_lg = "align-self-lg-baseline"
  let align_self_stretch_lg = "align-self-lg-stretch"
  let align_self_start_xl = "align-self-xl-start"
  let align_self_end_xl = "align-self-xl-end"
  let align_self_center_xl = "align-self-xl-center"
  let align_self_baseline_xl = "align-self-xl-baseline"
  let align_self_stretch_xl = "align-self-xl-stretch"

  let align_content_start = "align-content-start"
  let align_content_end = "align-content-end"
  let align_content_center = "align-content-center"
  let align_content_baseline = "align-content-baseline"
  let align_content_stretch = "align-content-stretch"
  let align_content_start_sm = "align-content-sm-start"
  let align_content_end_sm = "align-content-sm-end"
  let align_content_center_sm = "align-content-sm-center"
  let align_content_baseline_sm = "align-content-sm-baseline"
  let align_content_stretch_sm = "align-content-sm-stretch"
  let align_content_start_md = "align-content-md-start"
  let align_content_end_md = "align-content-md-end"
  let align_content_center_md = "align-content-md-center"
  let align_content_baseline_md = "align-content-md-baseline"
  let align_content_stretch_md = "align-content-md-stretch"
  let align_content_start_lg = "align-content-lg-start"
  let align_content_end_lg = "align-content-lg-end"
  let align_content_center_lg = "align-content-lg-center"
  let align_content_baseline_lg = "align-content-lg-baseline"
  let align_content_stretch_lg = "align-content-lg-stretch"
  let align_content_start_xl = "align-content-xl-start"
  let align_content_end_xl = "align-content-xl-end"
  let align_content_center_xl = "align-content-xl-center"
  let align_content_baseline_xl = "align-content-xl-baseline"
  let align_content_stretch_xl = "align-content-xl-stretch"

  let flex_wrap = "flex-wrap"
end

module Float = struct
  let float_left = "float-left"
  let float_right = "float-right"
  let float_none = "float-none"
  let float_left_sm = "float-left-sm"
  let float_right_sm = "float-right-sm"
  let float_none_sm = "float-none-sm"
  let float_left_md = "float-left-md"
  let float_right_md = "float-right-md"
  let float_none_md = "float-none-md"
  let float_left_lg = "float-left-lg"
  let float_right_lg = "float-right-lg"
  let float_none_lg = "float-none-lg"
  let float_left_xl = "float-left-xl"
  let float_right_xl = "float-right-xl"
  let float_none_xl = "float-none-xl"
end

module Position = struct
  let static = "position-static"
  let relative = "position-relative"
  let absolute = "position-absolute"
  let fixed = "position-fixed"
  let sticky = "position-sticky"
  let fixed_top = "fixed-top"
  let fixed_bottom = "fixed-bottom"
  let sticky_top = "sticky-top"
end

module Sizing = struct
  let w25 = "w-25"
  let w50 = "w-50"
  let w75 = "w75"
  let w100 = "w-100"
  let h25 = "h-25"
  let h50 = "h-50"
  let h75 = "h75"
  let h100 = "h-100"
  let mw25 = "mw-25"
  let mw50 = "mw-50"
  let mw75 = "mw75"
  let mw100 = "mw-100"
  let mh25 = "mh-25"
  let mh50 = "mh-50"
  let mh75 = "mh75"
  let mh100 = "mh-100"
end

module Spacing = struct
  let m0 = "m-0"
  let m1 = "m-1"
  let m2 = "m-2"
  let m3 = "m-3"
  let m4 = "m-4"
  let m5 = "m-5"
  let mauto = "m-auto"
  let mt0 = "mt-0"
  let mt1 = "mt-1"
  let mt2 = "mt-2"
  let mt3 = "mt-3"
  let mt4 = "mt-4"
  let mt5 = "mt-5"
  let mtauto = "mt-auto"
  let mb0 = "mb-0"
  let mb1 = "mb-1"
  let mb2 = "mb-2"
  let mb3 = "mb-3"
  let mb4 = "mb-4"
  let mb5 = "mb-5"
  let mbauto = "mb-auto"
  let ml0 = "ml-0"
  let ml1 = "ml-1"
  let ml2 = "ml-2"
  let ml3 = "ml-3"
  let ml4 = "ml-4"
  let ml5 = "ml-5"
  let mlauto = "ml-auto"
  let mr0 = "mr-0"
  let mr1 = "mr-1"
  let mr2 = "mr-2"
  let mr3 = "mr-3"
  let mr4 = "mr-4"
  let mr5 = "mr-5"
  let mrauto = "mr-auto"
  let mx0 = "mx-0"
  let mx1 = "mx-1"
  let mx2 = "mx-2"
  let mx3 = "mx-3"
  let mx4 = "mx-4"
  let mx5 = "mx-5"
  let mxauto = "mx-auto"
  let my0 = "my-0"
  let my1 = "my-1"
  let my2 = "my-2"
  let my3 = "my-3"
  let my4 = "my-4"
  let my5 = "my-5"
  let myauto = "my-auto"

  let p0 = "p-0"
  let p1 = "p-1"
  let p2 = "p-2"
  let p3 = "p-3"
  let p4 = "p-4"
  let p5 = "p-5"
  let pauto = "p-auto"
  let pt0 = "pt-0"
  let pt1 = "pt-1"
  let pt2 = "pt-2"
  let pt3 = "pt-3"
  let pt4 = "pt-4"
  let pt5 = "pt-5"
  let ptauto = "pt-auto"
  let pb0 = "pb-0"
  let pb1 = "pb-1"
  let pb2 = "pb-2"
  let pb3 = "pb-3"
  let pb4 = "pb-4"
  let pb5 = "pb-5"
  let pbauto = "pb-auto"
  let pl0 = "pl-0"
  let pl1 = "pl-1"
  let pl2 = "pl-2"
  let pl3 = "pl-3"
  let pl4 = "pl-4"
  let pl5 = "pl-5"
  let plauto = "pl-auto"
  let pr0 = "pr-0"
  let pr1 = "pr-1"
  let pr2 = "pr-2"
  let pr3 = "pr-3"
  let pr4 = "pr-4"
  let pr5 = "pr-5"
  let prauto = "pr-auto"
  let px0 = "px-0"
  let px1 = "px-1"
  let px2 = "px-2"
  let px3 = "px-3"
  let px4 = "px-4"
  let px5 = "px-5"
  let pxauto = "px-auto"
  let py0 = "py-0"
  let py1 = "py-1"
  let py2 = "py-2"
  let py3 = "py-3"
  let py4 = "py-4"
  let py5 = "py-5"
  let pyauto = "py-auto"
end

module Align = struct
  let align_baseline = "align-baseline"
  let align_top = "align-top"
  let align_middle = "align-middle"
  let align_bottom = "align-bottom"
  let align_text_top = "align-text-top"
  let align_text_bottom = "align-text-bottom"
end