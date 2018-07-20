open StringCompat

type storage = [ `LocalStorage | `Cookie | `Temp ]

let cookie_name = "lang"
let localStorage_name = Jsloc.host() ^ ":lang"

let same s = s,s

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

let pcdata_s s = Tyxml_js.Html5.pcdata (s_ s)

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
