open Ocp_js.Html
open Js_utils

open Bs4.Utils
open Spacing
open Grid
open Display
open Flex

open Bs4.Items
open BForm
open Alert
open Button
open Modal

module ROW = struct

  let _210 x y =
    div ~a:[ a_class [row] ]
      [
        div ~a:[ a_class [cmd2] ] x;
        div ~a:[ a_class [cmd10] ] y;
      ]

  let _39 x y =
    div ~a:[ a_class [row] ]
      [
        div ~a:[ a_class [cmd3] ] x;
        div ~a:[ a_class [cmd9] ] y;
      ]

  let _48 x y =
    div ~a:[ a_class [row] ]
      [
        div ~a:[ a_class [cmd4] ] x;
        div ~a:[ a_class [cmd8] ] y;
      ]

  let _57 x y =
    div ~a:[ a_class [row] ]
      [
        div ~a:[ a_class [cmd5] ] x;
        div ~a:[ a_class [cmd7] ] y;
      ]

  let _66 x y =
    div ~a:[ a_class [row] ]
      [
        div ~a:[ a_class [cmd] ] x;
        div ~a:[ a_class [cmd] ] y;
      ]

  let _93 x y =
    div ~a:[ a_class [row] ]
      [
        div ~a:[ a_class [cmd9] ] x;
        div ~a:[ a_class [cmd3] ] y;
      ]



  let _444 x y z =
    div ~a:[ a_class [row] ]
      [
        div ~a:[ a_class [cmd] ] x;
        div ~a:[ a_class [cmd] ] y;
        div ~a:[ a_class [cmd] ] z;
      ]

end

module Make(S : sig
    val group_classes : string list
    val input_classes : string list
    val prepend_classes : string list
  end) = struct

  let select ?(width=180) ?selected ?(a=[])
      id ?(onselect=(fun _ -> ())) title options =
    let options =
      List.map
        (fun (name, tag) ->
           let a = [a_id tag; a_value tag] in
           let a = match selected with
             | None -> a
             | Some t when t = tag -> a_selected () :: a
             | Some _ -> a
           in
           option ~a @@ txt (Format.sprintf "%s" name)
        ) options
    in
    div ~a:[ a_class (input_group :: S.group_classes); a_id (id ^ "-form") ] [
      div ~a:[ a_class [input_group_prepend] ] [
        span ~a:[ a_class (input_group_text :: S.prepend_classes);
                  Printf.kprintf a_style "width:%dpx;" width ] [ txt title ]
      ];

      Ocp_js.Html.select
        ~a:( a_id (id ^ "-input") ::
             a_class (form_control :: S.input_classes) ::
             a_onchange (fun _e ->
                 let input_elt = find_component @@ id ^ "-input" in
                 let v = Manip.value input_elt in
                 onselect v; true) ::
             a)
        options
    ]

  let field ?(input_type=`Text) ?(placeholder="") ?(width=220)
      ?(a=[])
      id ~onchange title =
    div ~a:[ a_class (input_group :: S.group_classes); a_id (id ^ "-form") ] [
      div ~a:[ a_class [input_group_prepend] ] [
        span ~a:[ a_class (input_group_text :: S.prepend_classes);
                  Printf.kprintf a_style "width:%dpx;" width ] [ txt title ] ];
      Ocp_js.Html.input ~a:( a_input_type input_type ::
                 a_id (id ^ "-input") ::
                 a_class (form_control :: S.input_classes) ::
                 a_placeholder placeholder ::
                 a_onchange (fun _e -> onchange id; true) ::
               a) ()
    ]


  let textarea ?(placeholder="") ?(rows=3) ?(width=180) id ~onchange title =
    div ~a:[ a_class (input_group :: S.group_classes); a_id (id ^ "-form") ] [
      div ~a:[ a_class [input_group_prepend] ] [
        span ~a:[ a_class (input_group_text :: S.prepend_classes);
                  Printf.kprintf a_style "width:%dpx;" width
                ] [ txt title ] ];
      textarea ~a:[
        a_id (id ^ "-input");
        a_class (form_control :: S.input_classes);
        a_placeholder placeholder;
        a_onchange (fun _e -> onchange id; true);
        a_rows rows;
      ]
        (txt "")
    ]
end

include Make(struct
    let group_classes = [ my2 ]
    let input_classes = []
    let prepend_classes = []
  end)


let check checkers id =
  let check =
    try List.assoc id checkers with
    |  Not_found ->
      (fun _v -> Some (Printf.sprintf "No checker for %S" id))
  in
  let input_elt = find_component @@ id ^ "-input" in
  let value = Manip.value input_elt in
  match check value with
  | None -> Ok value
  | Some msg -> Error msg

let check_or_fail checkers id =
  match check checkers id with
  | Ok value -> value
  | Error msg -> failwith msg

let onchange checkers id =
  let v = check checkers id in
  let input_elt = find_component @@ id ^ "-input" in
  let help = Manip.by_id @@ id ^ "-help" in
  let container = find_component @@ id ^ "-form" in
  match v with
  | Ok _value ->
    (try
       Manip.removeClass input_elt is_invalid;
       Manip.addClass input_elt is_valid;
     with _ -> ());
    (match help with
     | Some help -> Manip.removeChild container help;
     | _ -> ())
  | Error msg ->
    (try
       Manip.removeClass input_elt is_valid;
       Manip.addClass input_elt is_invalid;
     with _ -> ());
    (match help with
     | Some help -> Manip.removeChild container help;
     | _ -> ());
    Manip.appendChild container
      (div ~a:[ a_class [input_group_append]; a_id (id ^ "-help") ]
         [ span ~a:[ a_class [input_group_text]] [ txt msg ] ])

let set ~id v = Manip.set_value (find_component (id ^ "-input")) v
let get id = Manip.value (find_component (id ^ "-input"))

type status =
  |  Danger
  | Success

(* Display a block showing a message *)

let status_class = function
  | Danger -> adanger
  | Success -> asuccess

let message form_id status message =
  let status = status_class status in
  let container = find_component @@ form_id ^ "-message" in
  let div_alert =
    div ~a:[ a_class [ Alert.alert; status; alert_dismissible] ] [
      button ~a:[ a_class [ aclose ]; a_data_dismiss modal ] [entity "times"];
      txt message ] in
  Manip.replaceChildren container [ div_alert ]

let messages form_id list =
  let divs =
    List.map (fun (status, message) ->
        let status = status_class status in
        div ~a:[ a_class [ Alert.alert; status; alert_dismissible] ] [
          button ~a:[ a_class [ aclose ]; a_data_dismiss Alert.alert ]
            [entity "times"];
          txt message ]) list in
  let container = find_component @@ form_id ^ "-message" in
  Manip.replaceChildren container divs

let wrap_onclick title ~onclick =
  fun _e ->
  try
    onclick ();false
  with exn ->
    Js_utils.log "Exception in button %s: %s" title
      (Printexc.to_string exn);
    false
(* Note: always return false to prevent the form from submitting *)

let button_without_confirm ?(a=[]) ?(classes=[btn_primary]) title ~onclick =
  let a =
    a_class (btn :: classes) ::
    a_onclick (wrap_onclick title ~onclick) ::
    a
  in
  button ~a [ txt title ]

let enable_button id =
  Manip.removeAttribute (find_component id) "disabled"

let disable_button id =
  Manip.removeAttribute (find_component id) "enabled"

let counter = ref 0

let button_with_confirm ?(a=[]) ?(classes=[btn_primary]) title ~onclick =
  let modal_id = Printf.sprintf "modal_%d" !counter in
  incr counter;
  let a =
    a_class (btn :: classes) ::
    Attribute.a_data_custom "toggle" modal ::
    Attribute.a_data_custom "target" ("#" ^ modal_id) ::
    a_onclick (fun _e -> false) ::
    a
  in
  let b =  button ~a [ txt title ] in
  let modal =
    div ~a:[ a_class [modal]; a_id modal_id ] [
      div ~a:[ a_class [modal_dialog]] [
        div ~a:[ a_class [modal_content]] [
          div ~a:[ a_class [modal_header]] [
            h4 ~a:[ a_class [modal_title]] [txt title ];
            button  ~a:[ a_class [ aclose]; a_data_dismiss modal;]
              [ entity "times" ];
          ];
          div ~a:[ a_class [modal_body]]  [
            txt "Confirm this action"
          ];
          div ~a:[ a_class [d_flex; justify_between]; a_style "height:50px;" ]
            [
              div ~a:[ a_class [modal_footer]] [
                button ~a:[ a_class [btn;btn_success]; a_data_dismiss modal;
                            a_onclick (wrap_onclick title ~onclick);
                          ]
                  [ txt "Confirm" ]
              ];
              div ~a:[ a_class [modal_footer]] [
                button ~a:[ a_class [btn; btn_danger];
                            a_data_dismiss modal]
                  [ txt "Cancel" ]
              ]
            ]
        ]
      ]]
  in
  [
    b;
    modal
  ]

let button = button_without_confirm
