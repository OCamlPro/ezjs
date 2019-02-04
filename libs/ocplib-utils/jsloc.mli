
val path : unit -> string list
val path_string : unit -> string
val args : unit -> (string * string) list
val proto : unit -> string (* "http://" or "https://" *)
val url : unit -> Ocp_js.Url.url
val host : unit -> string

val find_arg : string -> string option
val set_args : (string * string) list -> unit
val set_url : Ocp_js.Url.url -> unit

val lang : unit -> string option (* None or Some "fr" *)
