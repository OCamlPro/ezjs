open Notifications_types
open Json_encoding


let subresource_uris =
  conv
    (fun _ -> assert false)
    (fun s -> s)
    (obj1
       (req "media" string)
    )


(* Convert some fields to sumtypes ? *)
let success_callback_encoding =
  conv
    (fun _ -> assert false) (* only destructs*)
    (fun (ans_sid, ans_date_created, ans_date_updated, ans_date_sent,
          ans_account_sid, ans_nb_to, ans_nb_from, ans_messaging_service_sid,
          ans_body, ans_status, ans_num_segments, ans_num_media, ans_direction,
          ans_api_version, ans_price, ans_price_unit, ans_error_code,
          ans_error_message, ans_uri, ans_subresource_uris) ->
      { ans_sid;
        ans_date_created;
        ans_date_updated;
        ans_date_sent;
        ans_account_sid;
        ans_nb_to;
        ans_nb_from;
        ans_messaging_service_sid;
        ans_body;
        ans_status;
        ans_num_segments;
        ans_num_media;
        ans_direction;
        ans_api_version;
        ans_price;
        ans_price_unit;
        ans_error_code;
        ans_error_message;
        ans_uri;
        ans_subresource_uris})
    (EzEncoding.obj20
       (req "sid" string)
       (req "date_created" string)
       (req "date_updated" string)
       (req "date_sent" (option string))
       (req "account_sid" string)
       (req "to" string)
       (req "from" string)
       (req "messaging_service_sid" (option string))
       (req "body" string)
       (req "status" string)
       (req "num_segments" string)
       (req "num_media" string)
       (req "direction" string)
       (req "api_version" string)
       (req "price" (option string))
       (req "price_unit" string)
       (req "error_code" (option string))
       (req "error_message" (option string))
       (req "uri" string)
       (req "subresource_uris" subresource_uris)
    )


let errors_encoding =
  conv
    (fun {err_code; err_msg; more_info; status} ->
       (err_code, err_msg, more_info, status))
    (fun (err_code, err_msg, more_info, status) ->
       {err_code; err_msg; more_info; status})
    (obj4
       (req "code" int)
       (req "message" string)
       (req "more_info" string)
       (req "status" int)
    )

let make_content params =
  "From=%2B" ^ Uri.pct_encode params.nb_from ^
  "&To=%2B" ^ Uri.pct_encode params.nb_to ^
  "&Body=" ^ Uri.pct_encode params.body

let destruct_success = EzEncoding.destruct success_callback_encoding
let destruct_errors = EzEncoding.destruct errors_encoding

let encodings = {
  name = "notifications_encodings";
  input = make_content;
  ok_output = destruct_success;
  ko_output = destruct_errors;
}
