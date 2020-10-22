

(* [Xhr.get msg url ok_handler] *)
val get :
  ?error:(int -> unit) ->
  string -> string -> (string -> unit) -> unit

(* [Xhr.post msg url ok_handler] *)
val post :
  ?content_type:string ->
  ?content:string ->
  ?error:(int -> unit) ->
  string -> string -> (string -> unit) -> unit
