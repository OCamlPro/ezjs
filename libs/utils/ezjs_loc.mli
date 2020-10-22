
val path : unit -> string list
val path_string : unit -> string
val args : unit -> (string * string) list
val proto : unit -> string (* "http://" or "https://" *)
val url : unit -> Js_of_ocaml.Url.url
val host : unit -> string

val find_arg : string -> string option
val set_args : (string * string) list -> unit
val set_url : Js_of_ocaml.Url.url -> unit

val lang : unit -> string option (* None or Some "fr" *)
