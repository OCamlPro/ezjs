type key = string
type value = string

(** Gets browser cookies and returns a [(key, value) list].  *)
val all : unit -> (key * value) list

(** Sets browser cookies. Expiration time is one year by default. *)
val set : key -> value -> unit

(** Sets browser cookies with expiration time. *)
val set_with_timeout : key -> value -> Js_of_ocaml.Js.date Js_of_ocaml.Js.t -> unit

val clear : key -> unit
