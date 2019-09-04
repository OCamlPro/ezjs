module E = Notifications_encoding
module T = Notifications_types
module C = Notifications_curl

open Notifications_config

(* As we use authentification through the URL itself, the format of endpoint is:
   https://[SID]:[token]@api.twilio.com/2010-04-01/Accounts/[SID]/Messages.json.
*)
let send_res ~account_sid ~auth_token ~nb_from ~nb_to ~body =
  let params = {T.nb_from; nb_to; body; } in
  let endpoint =
    "https://" ^
    account_sid ^
    ":" ^
    auth_token ^
    "@api.twilio.com" ^
    "/2010-04-01/Accounts/" ^
    account_sid ^
    "/Messages.json"
  in
  let content = params in
  C.post
    ~endpoint
    ~content
    ~encodings:E.encodings

let send ~conf ?(nb_from=None) ~nb_to body =
  let account_sid = conf.account_sid in
  let auth_token = conf.auth_token in
  let nb_from = match nb_from with
    | None -> conf.default_from
    | Some nb -> nb
  in
  match send_res ~account_sid ~auth_token ~nb_from ~nb_to ~body with
  | None ->
    Format.eprintf "Error without message!"

  | Some s ->
    match s with
    | Error e ->
      Format.eprintf "%s" (T.pp_error e)

    | Ok msg ->
      (*Debugging, uncomment below to get the json*)
      Format.eprintf "%s" (T.pp_callback_content msg)
