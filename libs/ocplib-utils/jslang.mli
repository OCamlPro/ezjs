
val declare_table :
  string ->
  (string, string) Hashtbl.t ->
  unit
val declare_translations :
  string ->
  (string * string) list ->
  unit
val add_translations :
  string ->
  (string * string) list ->
  unit
val same : string -> string * string





type storage = [ `LocalStorage | `Cookie | `Temp ]

val set : ?set:storage -> StringCompat.StringMap.key -> unit
val get : unit -> string option

val s_ : string -> string
val pcdata_s : string -> [> `PCDATA ] Tyxml_js.Html5.elt

type string_id
val ss_ : string -> string_id
val t_ : string_id -> string
val pcdata_t : string_id -> [> `PCDATA ] Tyxml_js.Html5.elt

val string_ids : unit -> StringCompat.StringSet.t
