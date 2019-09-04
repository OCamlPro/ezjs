type params = {
  nb_from : string;
  nb_to : string;
  body : string;
}

type error = {
  err_code : int ;
  err_msg : string ;
  more_info : string;
  status: int;
}

type callback_content = {
  ans_sid : string;
  ans_date_created : string;
  ans_date_updated : string;
  ans_date_sent : string option;
  ans_account_sid : string;
  ans_nb_to : string;
  ans_nb_from : string;
  ans_messaging_service_sid : string option;
  ans_body : string;
  ans_status : string;
  ans_num_segments : string;
  ans_num_media : string;
  ans_direction : string;
  ans_api_version : string;
  ans_price : string option;
  ans_price_unit : string;
  ans_error_code: string option;
  ans_error_message : string option;
  ans_uri : string ;
  ans_subresource_uris : string
}

type ('input, 'ok_output, 'ko_output) request_info = {
  name : string;
  input : ('input -> string);
  ok_output : (string -> 'ok_output);
  ko_output : (string -> 'ko_output)
}


let pp_opt opt = match opt with
  | None -> "null"
  | Some s -> s


let pp_error err =
  Format.sprintf
    "Error code: %d\n \
     Error: %s\n \
     More_info: %s\n \
     Status:%d \n"
    err.err_code
    err.err_msg
    err.more_info
    err.status


let pp_callback_content clbk =
  Format.sprintf
    "SID: %s\n\
     Date created: %s\n\
     Date updated: %s\n\
     Date sent: %s\n\
     Account SID: %s\n\
     To: %s\n\
     From: %s\n\
     Messaging service SID: %s\n\
     Body: %s\n\
     Status: %s\n\
     Number of segments: %s\n\
     Number of media files: %s\n\
     Direction: %s\n\
     Api version: %s\n\
     Price: %s\n\
     Price unit: %s\n\
     Error code: %s\n\
     Error message: %s\n\
     URI: %s\n\
     Subresource URIs: %s\n"
    clbk.ans_sid
    clbk.ans_date_created
    clbk.ans_date_updated
    (pp_opt clbk.ans_date_sent)
    clbk.ans_account_sid
    clbk.ans_nb_to
    clbk.ans_nb_from
    (pp_opt clbk.ans_messaging_service_sid)
    clbk.ans_body
    clbk.ans_status
    clbk.ans_num_segments
    clbk.ans_num_media
    clbk.ans_direction
    clbk.ans_api_version
    (pp_opt clbk.ans_price)
    clbk.ans_price_unit
    (pp_opt clbk.ans_error_code)
    (pp_opt clbk.ans_error_message)
    clbk.ans_uri
    clbk.ans_subresource_uris
