module T = Notifications_types

(** aux function: when destructing the answser of a request fails *)
let destruct_request_failed ~http_code res err =
  Format.eprintf
    "Request ended with HTTP code %d, but I was not able to decode the \
     answer %s\n Raised Exn: %s\n" http_code res (Printexc.to_string err);
  assert false


(** aux function: success callack provided to get/post request *)
let success_callback res_ref encodings ans =
  try res_ref := Some (Ok (encodings.T.ok_output ans))
  with err -> destruct_request_failed ~http_code:200 ans err

(** aux function: error callack provided to get/post request *)
let error_callback url res_ref encodings httpc s_opt =
  match s_opt with
  | None ->
    Format.eprintf "Request failed with code %d. No result returned !" httpc;
    assert false

  | Some ans ->
    try
      if httpc > 200 && httpc <= 226 then
        begin
        (* In ezCurl, 2xx codes, with xx > 0 are interpreted as errors *)
        Format.eprintf
          "debug me: error_callback with HTTP RET code %d when sending \
           a request with URL %s!@." httpc url;
        res_ref := Some (Ok (encodings.T.ok_output ans))
      end
      else if httpc <> 200 then
        begin
          Format.eprintf
            "error %d when sending a request with URL %s!@." httpc url;
          res_ref := Some (Error (encodings.T.ko_output ans))
      end;
    with e ->
      destruct_request_failed ~http_code:httpc ans e



let pp_header headers =
  List.fold_left
    (fun acc (a,b) ->
      let header = Format.sprintf " -H '%s:%s'" a b in
      acc ^ header) "" headers

let post ~endpoint ~content ~encodings =
  let content_type = "application/x-www-form-urlencoded" in
  let headers = [("Content-Type", content_type)] in
  let res_ref = ref None in
  let content = encodings.T.input content in
  Format.eprintf "curl -X POST %s d '%s' %s\n@."
    endpoint content (pp_header headers);
  EzCurl.post encodings.T.name ~headers ~content_type ~content
    (EzAPI.TYPES.URL endpoint)
    ~error:(error_callback endpoint res_ref encodings)
    (success_callback res_ref encodings);
  !res_ref
