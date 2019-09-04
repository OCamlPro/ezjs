(** Parameters for Twilio API *)
type params = {
  nb_from : string; (* Twilio sender's phone number*)
  nb_to : string;   (* Receiver's phone number *)
  body : string;    (* Content of the message *)
}


(** Error record sent by Twilio *)
type error = {
  err_code : int ;
  err_msg : string ;
  more_info : string;
  status: int;
}

(** Record sent by Twilio when a message is successfully sent *)
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


(** Encodings for curl request *)
type ('input, 'ok_output, 'ko_output) request_info = {
  name : string;                        (* Name of the request *)
  input : ('input -> string);           (* Content encoder *)
  ok_output : (string -> 'ok_output);   (* Decoder in case of success *)
  ko_output : (string -> 'ko_output)    (* Decoder in case of failure*)
}


(** Pretty prints error answers from Twilio *)
val pp_error : error -> string

(** Pretty prints success answers from Twilio *)
val pp_callback_content : callback_content -> string
