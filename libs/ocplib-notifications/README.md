# ocplib-notifications

This library is based on the Twilio SMS API. To use this library, you will need a Twilio account.


## Example

Beforehand, you exported Twilio account SID, auth token and the default Twilio phone number you want to use in the following environment variables (name can be arbitrary) :
	- `NOTIFICATIONS_DEFAULT_FROM`
	- `NOTIFICATIONS_ACCOUNT_SID`
	- `NOTIFICATIONS_AUTH_TOKEN`
	
The config file is stored in `config/notifications_config.json` , which looks like this :
```json
{
	"default_from" : "NOTIFICATIONS_DEFAULT_FROM",
	"account_sid": "NOTIFICATIONS_ACCOUNT_SID",
	"auth_token" : "NOTIFICATIONS_AUTH_TOKEN"
}
```
 You want to send a SMS to yourself whenever a function raises an error. Your own number is `+33 01 10 20 10`.

```ocaml
let example () = 
  let conf = 
    Notifications_config.load ~file:"config/notifications_config.json"
  in
  try can_crash ()
  with err ->  
   let err_msg = "The function crashed !" in
   Notifications.send ~conf ~nb_to:"3301102010" err_msg;
   raise err
```
