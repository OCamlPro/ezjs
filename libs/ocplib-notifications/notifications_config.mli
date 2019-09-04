(** Configuration values for Twilio. *)
type t = {
  default_from : string; (* Twilio phone's sender *)
  account_sid : string;  (* Twilio account SID *)
  auth_token : string;   (* Twilio authentification token *)
}

(** Takes a config json file as an argument and returns the corresponding
    configuration variables. An example of a config file is given in the folder.

    The config file should specify in which environment variables the values
    are stored. The config file must have the following fields :
    - default_from
    - account_sid
    - auth_token *)
val load : file : string -> t
