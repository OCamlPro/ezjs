open Ezjs_tyxml
open Html
open Bs4.Utils
open BMisc
open Bs4.Items

type kind = Tab | Pill | Empty
type state = Active | Disabled | Inactive | Hidden
type 'a tnav = {
  id: string;
  title: 'a option -> Html_types.flow5_without_interactive elt;
  classes: string list;
  kind: kind;
  onshow: unit -> unit;
  mutable state: state;
  mutable first: bool;
}

let make ?(kind=Tab) ?(state=Inactive) ?(onshow=fun () -> ()) ?(classes=[])
    id title =
  {id; title; kind; state; onshow; classes; first = true}

let make_state_class = function
  | Active -> [ active; bshow ]
  | Disabled -> [ disabled ]
  | Inactive | Hidden -> []

let make_state_content_class = function
  | Active -> [ active ]
  | _ -> [ fade ]

let kind_str = function
  | Tab -> "tab"
  | Pill -> "pill"
  | Empty -> ""

let disable nav =
  Manip.addClass (find_component ("li-nav-" ^ nav.id)) disabled;
  let link = find_component ("nav-" ^ nav.id) in
  Manip.removeAttribute link "data-toggle";
  Manip.removeAttribute link "href"

let enable nav =
  Manip.removeClass (find_component ("li-nav-" ^ nav.id)) disabled;
  let link = find_component ("nav" ^ nav.id) in
  Manip.setAttribute link "data-toggle" (kind_str nav.kind);
  Manip.setAttribute link "href" ("#nav-content-" ^ nav.id)

let show nav = show (find_component ("li-nav-" ^ nav.id))
let hide nav = hide (find_component ("li-nav-" ^ nav.id))

let activate_nav nav =
  match nav.state with
  | Active | Hidden | Disabled -> ()
  | Inactive ->
    nav.state <- Active;
    Manip.addClass (find_component ("nav-" ^ nav.id)) active;
    Manip.removeClass (find_component ("nav-content-" ^ nav.id)) fade;
    Manip.addClass (find_component ("nav-content-" ^ nav.id)) active;
    Manip.addClass (find_component ("nav-content-" ^ nav.id)) bshow

let deactivate_nav nav =
  match nav.state with
  | Inactive | Hidden | Disabled -> ()
  | Active ->
    nav.state <- Inactive;
    Manip.removeClass (find_component ("nav-"  ^ nav.id)) active;
    Manip.removeClass (find_component ("nav-content-" ^ nav.id)) active;
    Manip.removeClass (find_component ("nav-content-" ^ nav.id)) bshow;
    Manip.addClass (find_component ("nav-content-" ^ nav.id)) fade

let change_arg ?(set_args=Ezjs_loc.set_args) nav =
  set_args ["nav", nav.id]

let make_tab_content nav content =
  div ~a:[ a_id ("nav-content-" ^ nav.id);
           a_class ([ Nav.tab_pane ] @ make_state_content_class nav.state)]
    [ content ]

let update_nav_title nav param =
  Manip.replaceChildren (find_component ("nav-" ^ nav.id)) [ nav.title (Some param) ]


module Make(S : sig type t end) = struct

  let current_nav : S.t tnav ref = ref (make "" (fun _ -> div []))
  let navs_list : S.t tnav list ref = ref []

  let change_nav ?set_args nav =
    deactivate_nav !current_nav;
    activate_nav nav;
    current_nav:= nav;
    change_arg ?set_args nav

  let update_nav ?set_args ?(once=true) nav =
    change_nav ?set_args nav;
    if (nav.first || not once) then (
      nav.first <- false;
      nav.onshow ())

  let init ?(find_arg=Ezjs_loc.find_arg) ?set_args ?value () =
    let f value =
      List.iter (fun nav -> if nav.id = value then update_nav ?set_args nav) !navs_list in
    match find_arg "nav", value with
      | _, Some value -> f value
      | Some value, _ -> f value
      | None, None ->
        List.iter (fun nav -> if nav.state = Active then update_nav ?set_args nav) !navs_list


  let make_nav ?(link=fun id -> a_href ("#nav-content-" ^ id)) ?set_args ?param ?once nav =
    let is_active_class =
      if nav.state = Disabled then [] else
        [ a_user_data "toggle" (kind_str nav.kind); link nav.id] in
    let is_hidden_attr =
      if nav.state <> Hidden then [] else [ a_style "display:none" ] in
    li ~a:([ a_class (Nav.nav_item :: nav.classes);
             a_id ("li-nav-" ^ nav.id);
             a_onshow (fun _e -> update_nav ?set_args ?once nav; true)]
           @ is_hidden_attr) [
      a ~a:([ a_id ("nav-" ^ nav.id);
              a_class (Nav.nav_link :: nav.classes @ make_state_class nav.state);
              Attribute.a_role (kind_str nav.kind);
              Attribute.a_aria "controls" ("nav-content-" ^ nav.id);
              Attribute.a_aria "selected" "true"
            ] @ is_active_class) [
        nav.title param
      ]
    ]

  let make_navs ?set_args ?(kind=Tab) ?(classes=[]) ?once navs =
    navs_list := navs;
    let classes = match kind with
      | Tab -> Nav.nav_tabs :: classes
      | Pill -> Nav.nav_pills :: classes
      | Empty -> classes in
    ul ~a:[ a_class (Nav.nav :: classes) ] @@
    List.map (fun nav -> if nav.state = Active then current_nav := nav;
               make_nav ?set_args ?once nav) navs

end
