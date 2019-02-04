open StringCompat

type storage = [ `LocalStorage | `Cookie | `Temp ]

let cookie_name = "lang"
let localStorage_name = Jsloc.host() ^ ":lang"

let same s = s,s

let strings_epoch = ref 1

let current_lang = ref None
let dictionaries = ref StringMap.empty
let current_dict = ref None

module Local = JsStorage.MakeLocal(struct
                             type t = string
                             let name = localStorage_name
                           end)

let set ?(set=`Temp) lang =
  current_lang := Some lang;
  current_dict := (try Some (StringMap.find lang !dictionaries) with _ -> None);
  incr strings_epoch;
  match set with
  | `LocalStorage ->
     Local.set lang;
     Cookie.clear cookie_name
  | `Temp -> ()
  | `Cookie ->
     Local.clear ();
     Cookie.set cookie_name lang

let declare_table lang dict =
  dictionaries := StringMap.add lang dict !dictionaries;
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
      StringMap.find lang !dictionaries
    with Not_found ->
      let dict = Hashtbl.create 133 in
      dictionaries := StringMap.add lang dict !dictionaries;
      dict
  in
  List.iter (fun (s1, s2) -> Hashtbl.add dict s1 s2) list;
  ()

let () =
  match Jsloc.lang () with
  | Some lang ->
     set ~set:`Cookie lang
  | None ->
  let cookies = Cookie.all () in
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
          Js_utils.log "Jslang: no %s-translation for %S" lang s;
          s

let txt_s s = Ocp_js.Html.txt (s_ s)
let pcdata_s s = txt_s s

type string_id = {
  id : string ;
  mutable translation : string option ;
  mutable epoch : int ;
}

let string_ids = ref StringMap.empty
let ss_ id =
  try
    StringMap.find id !string_ids
  with Not_found ->
    let s = { id ; translation = None ; epoch = 0 } in
    string_ids := StringMap.add id s !string_ids;
    s

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

let txt_t s = Ocp_js.Html.txt (t_ s)
let pcdata_t s = txt_t s

let string_ids() =
  StringMap.fold (fun ele _ set ->
      StringSet.add ele set) !string_ids StringSet.empty
