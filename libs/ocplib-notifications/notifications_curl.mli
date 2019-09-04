module T = Notifications_types

(** Takes an endpoint, a content and encodings, and makes the corresponding
    cURL request. By default, Content-Type is specified as
    x-www-form-urlencoded.*)
val post :
  endpoint : string ->
  content : T.params ->
  encodings : (T.params, T.callback_content, T.error) T.request_info ->
  (T.callback_content, T.error) result option
