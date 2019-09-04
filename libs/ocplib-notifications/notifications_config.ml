open Json_encoding

type t = {
  default_from : string;
  account_sid : string;
  auth_token : string;
}

let req_value_from_opt env_v =
  match Sys.getenv_opt env_v with
  | Some v -> v
  | None ->
    Format.eprintf
      "No value provided for variable %S. You may want to modify your config \
       file, or export the value in the enviroment variable %S@."
      env_v env_v;
    exit 1

let config_encoding =
  conv
    (fun _ -> assert false) (* Should only destruct json *)
    (fun e ->
       let (default_from, account_sid, auth_token) = e in
       let default_from = req_value_from_opt default_from in
       let account_sid = req_value_from_opt account_sid in
       let auth_token = req_value_from_opt auth_token in
       {default_from;
        account_sid;
        auth_token}
    )
    (obj3
       (req "default_from" string)
       (req "account_sid" string)
       (req "auth_token" string)
    )

let load ~file =
  Format.eprintf "Loading config from file %S ...@." file;
  try
    let ic = open_in file in
    let json = Ezjsonm.from_channel ic in
    close_in ic;
    destruct config_encoding json
  with exn ->
    Format.eprintf "cannot read config file %S:\nError: %s\n@."
      file (Printexc.to_string exn);
    exit 1
