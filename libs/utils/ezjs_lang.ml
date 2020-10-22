type storage = [ `LocalStorage | `Cookie | `Temp ]

let cookie_name = "lang"
let localStorage_name = Ezjs_loc.host() ^ ":lang"

let same s = s,s

let strings_epoch = ref 1

let current_lang = ref None
let dictionaries = Hashtbl.create 512
let current_dict = ref None

module Local = Ezjs_storage.MakeLocal(struct
    type t = string
    let name = localStorage_name
  end)

let set ?(set=`Temp) lang =
  current_lang := Some lang;
  current_dict := (try Some (Hashtbl.find dictionaries lang) with _ -> None);
  incr strings_epoch;
  match set with
  | `LocalStorage ->
    Local.set lang;
    Ezjs_cookie.clear cookie_name
  | `Temp -> ()
  | `Cookie ->
    Local.clear ();
    Ezjs_cookie.set cookie_name lang

let declare_table lang dict =
  Hashtbl.add dictionaries lang dict ;
  match !current_lang with
  | None -> ()
  | Some lang -> set lang

let declare_translations lang list =
  let dict = Hashtbl.create 133 in
  List.iter (fun (s1, s2) -> Hashtbl.add dict s1 s2) list;
  declare_table lang dict

let add_translations lang list =
  let dict =
    try
      Hashtbl.find dictionaries lang
    with Not_found ->
      let dict = Hashtbl.create 133 in
      Hashtbl.add dictionaries lang dict ;
      dict
  in
  List.iter (fun (s1, s2) -> Hashtbl.add dict s1 s2) list;
  ()

let () =
  match Ezjs_loc.lang () with
  | Some lang ->
    set ~set:`Cookie lang
  | None ->
    let cookies = Ezjs_cookie.all () in
    let lang =
      try
        Some (List.assoc cookie_name cookies)
      with Not_found ->
        Local.get ()
    in
    match lang with
    | Some lang -> set lang
    | None -> ()

let get () = !current_lang


let s_ s =
  match !current_dict with
  | None -> s
  | Some dict ->
    try
      Hashtbl.find dict s
    with Not_found ->
    match !current_lang with
    | None -> assert false
    | Some lang ->
      Ezjs_tyxml.log "Jslang: no %s-translation for %S" lang s;
      s

let txt_s s = Ezjs_tyxml.Html.txt (s_ s)
let pcdata_s s = txt_s s

type string_id = {
  id : string ;
  mutable translation : string option ;
  mutable epoch : int ;
}

let ss_ id =
  { id ; translation = None ; epoch = 0 }

let t_ s =
  let epoch = !strings_epoch in
  if s.epoch = epoch then
    match s.translation with
    | None -> assert false
    | Some tr -> tr
  else
    let tr = s_ s.id in
    s.translation <- Some tr;
    s.epoch <- epoch;
    tr

let id_ s = s.id

let txt_t s = Ezjs_tyxml.Html.txt (t_ s)
let pcdata_t s = txt_t s
