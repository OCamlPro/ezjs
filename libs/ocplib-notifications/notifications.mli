(** [send ~config ~nb_from ~nb_to msg] sends a SMS from the number [nb_from]
    to the number [nb_to] with the message [msg] using the [config] loaded
    with Notifications_config.load.

    By default, if nb_from is not specified, the number specified through the
    config file is used. The number must be one provided by Twilio.

    Phone numbers must have the following format :
    (countrycode)(numbers). The '+' is not needed.*)
val send :
  conf : Notifications_config.t ->
  ?nb_from : string option ->
  nb_to : string ->
  string -> (* Body *)
  unit
