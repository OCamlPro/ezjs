
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

val set : ?set:storage -> string -> unit
val get : unit -> string option

val s_ : string -> string
val pcdata_s : string -> [> `PCDATA ] Ezjs_tyxml.elt
val txt_s : string -> [> `PCDATA ] Ezjs_tyxml.elt

type string_id
val ss_ : string -> string_id
val t_ : string_id -> string
val pcdata_t : string_id -> [> `PCDATA ] Ezjs_tyxml.elt
val txt_t : string_id -> [> `PCDATA ] Ezjs_tyxml.elt
val id_ : string_id -> string
