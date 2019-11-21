open Ocp_js.Html

module Misc = struct
  let lead = "lead"
end

module Attributes = struct
  (** WAI-ARIA attributes (deprecated by Tyxml_js >= 4.1.0 *)
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

module Icon = struct

  let right_icon () = span ~a:[ a_class [ "fas"; "fa-arrow-right" ] ] []
  let left_icon () = span ~a:[ a_class [ "fas"; "fa-arrow-left" ] ] []
  let down_icon () = span ~a:[ a_class [ "fas"; "fa-arrow-down" ] ] []
  (* let double_right_icon () = span ~a:[ a_class [ "fa"; "fa-angle-double-right" ] ] []
   * let double_left_icon () = span ~a:[ a_class [ "fa"; "fa-angle-double-left" ] ] [] *)
  let double_right_icon () = txt "»"
  let double_left_icon () = txt "«"
  let hourglass_icon () = span ~a:[ a_class [ "fas"; "fa-hourglass-half" ] ] []
  let clock_icon () = span ~a:[ a_class [ "fas"; "fa-clock" ] ] []
  let code_branch_icon () = span ~a:[ a_class [ "fas"; "fa-code-branch" ] ] []
  let exchange_icon () = span ~a:[ a_class [ "fas"; "fa-exchange-alt"] ] [ ]
  let bars_icon () = span ~a:[ a_class [ "fas"; "fa-bars"] ] [ ]
  let chart_line_icon () = span ~a:[ a_class [ "fas"; "fa-chart-line"] ] [ ]
  let cube_icon () = span ~a:[ a_class [ "fas"; "fa-cube"] ] [ ]
  let cubes_icon () = span ~a:[ a_class [ "fas"; "fa-cubes"] ] [ ]
  let chart_pie_icon () = span ~a:[ a_class [ "fas"; "fa-chart-pie"] ] [ ]
  let signal_icon () = span ~a:[ a_class [ "fas"; "fa-signal"] ] [ ]
  let code_icon () = span ~a:[ a_class [ "fas"; "fa-code"] ] [ ]
  let handshake_icon () = span ~a:[ a_class [ "fas"; "fa-handshake"] ] [ ]
  let link_icon () = span ~a:[ a_class [ "fas"; "fa-link"] ] [ ]
  let check_icon () = span ~a:[ a_class [ "fas"; "fa-check"] ] [ ]
  let database_icon () = span ~a:[ a_class [ "fas"; "fa-database"] ] [ ]
  let bill_icon () = span ~a:[ a_class [ "fas"; "fa-money-bill-wave"] ] []
  let cross_icon () = span ~a:[ a_class [ "fas"; "fa-times"] ] []
  let cycle_icon () = span ~a:[ a_class [ "fas"; "fa-undo"] ] []
  let liquidity_icon () =
    img ~a:[ a_class [ "liquidity-icon" ] ]
      ~src:"images/liquidity_icon.svg" ~alt:"Liquidity" ()
  let clipboard_icon () = span ~a:[ a_class [ "fas"; "fa-clipboard" ] ] []

  let space_icon () = entity "nbsp"
  let tz_icon () = entity "#xa729"
  let mu_icon () = entity "#956"
  let right_left_arrow_icon () = entity "#8644"
  let times_icon () = entity "times"
  let no_break_space_icon () = entity "#160"
  let snowflake () = entity "#x2744"
  let thumb_up () = entity "#x1f44d"
  let beta () = entity "#x3b2"
  let chart_up () = entity "#x1F4C8"
  let chart_down () =  entity "#x1F4C9"

  let params_icon () = span ~a:[ a_class [ "fas"; "fa-cog"] ] []
  let folder_icon () = span ~a:[ a_class [ "fas"; "fa-folder-open"] ] []
  let account_icon () = span ~a:[ a_class [ "fas"; "fa-user-circle"] ] []
  let secret_icon () = span ~a:[ a_class [ "fas"; "fa-user-secret"] ] []
  let arrow_up_icon () = span ~a:[ a_class [ "fas"; "fa-arrow-alt-circle-up"] ] []
  let slots_icon () = span ~a:[ a_class [ "fab"; "fa-buromobelexperte"] ] []
  let stamp_icon () = span ~a:[ a_class [ "fas"; "fa-stamp" ] ] []
  let astronaut_icon () = span ~a:[ a_class [ "fas"; "fa-user-astronaut" ] ] []
  let burn_icon () = span ~a:[ a_class [ "fas"; "fa-burn" ] ] []
  let cookie_icon () = span ~a:[ a_class [ "fas"; "fa-cookie-bite" ] ] []
  let wallet_icon () = span ~a:[ a_class [ "fas"; "fa-wallet" ] ] []
  let balance_icon () = span ~a:[ a_class [ "fas"; "fa-balance-scale" ] ] []
  let uncle_icon () = span ~a:[ a_class [ "fas"; "fa-code-branch" ] ] []
  let ruler_icon () = span ~a:[ a_class [ "fas"; "fa-ruler" ] ] []
  let manager_icon () = span ~a:[ a_class [ "fas"; "fa-user-shield" ] ] []
  let originator_icon () = span ~a:[ a_class [ "fas"; "fa-user-edit" ] ] []
  let priority_icon () = span ~a:[ a_class [ "fas"; "fa-angle-double-up" ] ] []
  let deposit_icon () = span ~a:[ a_class [ "fas"; "fa-hand-holding" ] ] []
  let spinner_icon () = span ~a:[ a_class [ "fas"; "fa-spinner" ] ] []
  let ghost_icon () = span ~a:[ a_class [ "fas"; "fa-snapchat-ghost" ] ] []
  let image_icon () = span ~a:[ a_class [ "fas"; "fa-image"] ] []
  let camera_icon () = span ~a:[ a_class [ "fas"; "fa-camera"] ] []
  let play_icon () = span ~a:[ a_class ["fas"; "fa-play"] ] []
  let stop_icon () = span ~a:[ a_class ["fas"; "fa-stop"] ] []
  let exclamation_icon () = span ~a:[ a_class ["fas"; "fa-exclamation-triangle"] ] []

  let number_icon icon () = span [txt "# "; icon ()]
  let number_icon_u icon () =  number_icon icon
  let glyph name = span ~a:[ a_class ["glyphicon"; "glyphicon-" ^ name ] ] []
  let glyph_u name () = glyph name
  let fas name = span ~a:[ a_class ["fas"; "fa-" ^ name ] ] []
  let fas_u name () = fas name
end

module Grid = struct
  let container = "container"
  let container_fluid = "container-fluid"
  let row = "row"

  let xs_size = 768
  let sm_size = 992
  let md_size = 1200
  let lg_size = 1500
  let xl_size = 1800

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

  let cxs1 = "col-xs-1"
  let cxs2 = "col-xs-2"
  let cxs3 = "col-xs-3"
  let cxs4 = "col-xs-4"
  let cxs5 = "col-xs-5"
  let cxs6 = "col-xs-6"
  let cxs7 = "col-xs-7"
  let cxs8 = "col-xs-8"
  let cxs9 = "col-xs-9"
  let cxs10 = "col-xs-10"
  let cxs11 = "col-xs-11"
  let cxs12 = "col-xs-12"

  let clgoffset0 = "col-lg-offset-0"
  let clgoffset1 = "col-lg-offset-1"
  let clgoffset2 = "col-lg-offset-2"
  let clgoffset3 = "col-lg-offset-3"
  let clgoffset4 = "col-lg-offset-4"
  let clgoffset5 = "col-lg-offset-5"
  let clgoffset6 = "col-lg-offset-6"
  let clgoffset7 = "col-lg-offset-7"
  let clgoffset8 = "col-lg-offset-8"
  let clgoffset9 = "col-lg-offset-9"
  let clgoffset10 = "col-lg-offset-10"
  let clgoffset11 = "col-lg-offset-11"
  let clgoffset12 = "col-lg-offset-12"

  let cmdoffset0 = "col-md-offset-0"
  let cmdoffset1 = "col-md-offset-1"
  let cmdoffset2 = "col-md-offset-2"
  let cmdoffset3 = "col-md-offset-3"
  let cmdoffset4 = "col-md-offset-4"
  let cmdoffset5 = "col-md-offset-5"
  let cmdoffset6 = "col-md-offset-6"
  let cmdoffset7 = "col-md-offset-7"
  let cmdoffset8 = "col-md-offset-8"
  let cmdoffset9 = "col-md-offset-9"
  let cmdoffset10 = "col-md-offset-10"
  let cmdoffset11 = "col-md-offset-11"
  let cmdoffset12 = "col-md-offset-12"

  let csmoffset0 = "col-sm-offset-0"
  let csmoffset1 = "col-sm-offset-1"
  let csmoffset2 = "col-sm-offset-2"
  let csmoffset3 = "col-sm-offset-3"
  let csmoffset4 = "col-sm-offset-4"
  let csmoffset5 = "col-sm-offset-5"
  let csmoffset6 = "col-sm-offset-6"
  let csmoffset7 = "col-sm-offset-7"
  let csmoffset8 = "col-sm-offset-8"
  let csmoffset9 = "col-sm-offset-9"
  let csmoffset10 = "col-sm-offset-10"
  let csmoffset11 = "col-sm-offset-11"
  let csmoffset12 = "col-sm-offset-12"

  let cxsoffset0 = "col-xs-offset-0"
  let cxsoffset1 = "col-xs-offset-1"
  let cxsoffset2 = "col-xs-offset-2"
  let cxsoffset3 = "col-xs-offset-3"
  let cxsoffset4 = "col-xs-offset-4"
  let cxsoffset5 = "col-xs-offset-5"
  let cxsoffset6 = "col-xs-offset-6"
  let cxsoffset7 = "col-xs-offset-7"
  let cxsoffset8 = "col-xs-offset-8"
  let cxsoffset9 = "col-xs-offset-9"
  let cxsoffset10 = "col-xs-offset-10"
  let cxsoffset11 = "col-xs-offset-11"
  let cxsoffset12 = "col-xs-offset-12"
end

module Panel = struct
  let panel = "panel"
  let panel_primary = "panel-primary"
  let panel_heading = "panel-heading"
  let panel_body = "panel-body"
  let panel_title = "panel-title"
  let panel_default = "panel-default"
  let panel_footer = "panel-footer"


  (** Helpers *)
  let make_panel
      ?(panel_class=[]) ?(panel_id) (* panel info *)
      ?(panel_heading_class=[]) ?panel_heading_id (* panel heading info *)
      ?(panel_title_class=[]) ?(panel_title_content) ?(panel_title_extra=[]) (* panel title info *)
      ?(panel_body_class=[]) ?panel_body_id ?(panel_body_content=[]) (* panel body info *)
      ?(panel_footer_class=[]) ?panel_footer_id ?panel_footer_content (* panel footer info *)
      () =
    let unopt_id = function None -> [] | Some id -> [ a_id id ] in
    let panel_id, panel_heading_id, panel_body_id, panel_footer_id =
      unopt_id panel_id, unopt_id panel_heading_id,
      unopt_id panel_body_id, unopt_id panel_footer_id in
    let panel_title_content = match panel_title_content with
      | None -> []
      | Some elt ->
        List.iter (Js_utils.Manip.addClass elt) (panel_title :: panel_title_class) ;
        [ elt ] in
    match panel_title_content, panel_footer_content with
    | [], None ->
      div ~a:(a_class (panel :: panel_primary :: panel_class) :: panel_id) [
        div ~a:(a_class (panel_body :: panel_body_class) :: panel_body_id)
          panel_body_content
      ]
    | _, None ->  (* TODO panel_id a ajouter *)
      div ~a:(a_class (panel :: panel_primary :: panel_class) :: panel_id) [
        div ~a:(a_class (panel_heading :: panel_heading_class) :: panel_heading_id) (
          panel_title_content @ panel_title_extra) ;
        div ~a:(a_class (panel_body :: panel_body_class) :: panel_body_id)
          panel_body_content
      ]
    | [], Some footer_content ->
      div ~a:(a_class (panel :: panel_primary :: panel_class) :: panel_id) [
        div ~a:(a_class (panel_body :: panel_body_class) :: panel_body_id)
          panel_body_content;
        div ~a:(a_class (panel_footer :: panel_footer_class) :: panel_footer_id)
          footer_content
      ]
    | _, Some footer_content ->
      div ~a:(a_class (panel :: panel_primary :: panel_class) :: panel_id) [
        div ~a:(a_class (panel_heading :: panel_heading_class) :: panel_heading_id) (
          panel_title_content @ panel_title_extra) ;
        div ~a:(a_class (panel_body :: panel_body_class) :: panel_body_id)
          panel_body_content;
        div ~a:(a_class (panel_footer :: panel_footer_class) :: panel_footer_id)
          footer_content
      ]

end

module Table = struct
  let btable = "table"
  let btable_bordered = "table-bordered"
  let btable_responsive = "table-responsive"
  let btable_striped = "table-striped"
end

module Modal = struct
  let modal = "modal"
  let modal_header = "modal-header"
  let modal_title = "modal-title"
  let modal_body = "modal-body"
  let modal_content = "modal-content"
  let modal_footer = "modal-footer"
  let modal_dialog = "modal-dialog"
  let modal_dialog_centered = "modal-dialog-centered"

end

module Input = struct
  let input_group = "input-group"
  let input_group_sm = "input-group-sm"
  let input_group_btn = "input-group-btn"
  let form_control = "form-control"
end

module Button = struct
  let btn = "btn"
  let btn_primary = "btn-primary"
  let btn_secondary = "btn-secondary"
  let btn_default = "btn-default"
  let btn_group = "btn-group"
  let btn_xs = "btn-xs"
  let btn_sm = "btn-sm"
  let btn_md = "btn-md"
  let btn_lg = "btn-lg"
  let dropdown_menu = "dropdown-menu"
  let dropdown_toggle = "dropdown-toggle"
end

module Color = struct
  let blue = "blue"
  let green = "green"
  let grey = "grey"
  let red = "red"
  let white = "white"
end

module Form = struct
  let form_group = "form-group"
  let form_control = "form-control"
  let form_inline = "form-inline"
end

module Menu = struct

  type menu =
    | Dropdown of string list *
                    Html_types.span_content_fun Ocp_js.elt list  *
                  menu list * bool
    | Link of string list *
                string * Html_types.flow5_without_interactive Ocp_js.elt * bool
    | Link2 of string list *
                Html_types.a_attrib attrib list * Html_types.flow5_without_interactive Ocp_js.Html.elt * bool
    | Action of string list *
                  (unit -> unit) *
                    Html_types.flow5_without_interactive Ocp_js.elt
    | Separator of string list
    | Header of string list * string
    | Generic of string list * Html_types.li_content_fun Ocp_js.elt list

  let rec bootstrap_menu =
    function
    | Dropdown (classes, title, items, disabled) ->
       li ~a:[ a_class ("dropdown" :: classes) ]
          [
            a ~a:[
              a_class (["dropdown-toggle"] @
                       if disabled then ["disabled"] else []);
                a_href "#";
                a_user_data "toggle" "dropdown";
                Attributes.a_role "button";
                Attributes.a_aria "haspopup" "true";
                Attributes.a_aria "expanded" "false";
              ]
              (span title :: [txt " "; span ~a:[ a_class ["caret"]] []]);
            ul ~a:[ a_class [ "dropdown-menu" ] ]
               (List.map bootstrap_menu items)
          ]
    | Link (classes, url, s, disabled) ->
      li ~a:[ a_class (if disabled then [ "disabled" ] else [] ) ] [
        a ~a:[ a_class classes; a_href url ] [s]
      ]
    | Link2 (classes, attribs, s, disabled) ->
      li ~a:[ a_class (if disabled then [ "disabled" ] else [] ) ] [
        a ~a:([ a_class classes ] @ attribs) [s]
      ]
    | Action (classes, f, s) ->
      li [
        a ~a:[ a_class classes;
               a_onclick (fun _ev ->  f (); false) ] [s]
      ]
    | Separator classes ->
      li ~a:[ a_class ("divider" :: classes); Attributes.a_role "separator" ] []
    | Header (classes, header) ->
      li ~a:[ a_class ("dropdown-header" :: classes) ] [ txt header ]
    | Generic (classes, elts) ->
      li ~a:[ a_class classes ] elts


  let bootstrap_dropdown_button ~btn_class ~ctn_class id content menus =
    div ~a:[ a_class (Button.btn_group :: ctn_class) ] [
      button ~a:[ a_button_type `Button; a_id id;
                  a_class ([ Button.btn; Button.dropdown_toggle ] @ btn_class);
                  Attributes.a_data_toggle "dropdown";
                  Attributes.a_aria "haspopup" "true";
                  Attributes.a_aria "expanded" "false"; ]
        content;
      ul ~a:[ a_class [ Button.dropdown_menu ] ] (List.map bootstrap_menu menus)
    ]
end

module Align = struct
  let text_center = "text-center"
  let text_right = "text-right"
end
