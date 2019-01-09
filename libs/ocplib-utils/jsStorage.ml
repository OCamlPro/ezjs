open Ocp_js

let local_args = ref []

let get_storage () =
  try
    match Js.Optdef.to_option Dom_html.window##localStorage with
    | None -> raise Not_found
    | Some t -> t
  with exn ->
    Js_utils.warn
      "can't access to localStorage.\n%s"
      (Printexc.to_string exn);
    raise Not_found

module MakeLocal(V: sig type t val name: string end) :
sig
  val set : V.t -> unit
  val get : unit -> V.t option
  val clear : unit -> unit
end = struct

  let () =
    if List.mem V.name !local_args then
      Js_utils.warn "Duplicate key in LocalStorage: %s" V.name
    else
      local_args := V.name :: !local_args

  let name = Js.string V.name

  let get () =
    try
      let s = get_storage () in
      match Js.Opt.to_option (s##getItem(name)) with
      | None -> None
      | Some s -> Some (Json.unsafe_input s : V.t)
    with Not_found -> None

  let set v =
    try
      let s = get_storage () in
      let str = Json.output (v : V.t) in
      s##setItem(name, str)
    with Not_found -> ()

  let clear () =
    try
      let s = get_storage () in
      s##removeItem(name)
    with Not_found -> ()

end
