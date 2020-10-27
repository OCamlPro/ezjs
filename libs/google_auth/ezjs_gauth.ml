open Ezjs_min
open Promise

class type basic_profile_js = object
  method getId : js_string t meth
  method getName : js_string t meth
  method getGivenName : js_string t optdef meth
  method getFamilyName : js_string t optdef meth
  method getImageUrl : js_string t optdef meth
  method getEmail : js_string t meth
end

class type auth_response_js = object
  method access_token_ : js_string t prop
  method id_token_ : js_string t prop
  method scope : js_string t prop
  method expires_in_ : int prop
  method first_issued_at_ : int prop
  method expires_at_ : int prop
end

class type signin_options_js = object
  method prompt : js_string t optdef readonly_prop
  method scope : js_string t optdef readonly_prop
  method ux_mode_ : js_string t optdef readonly_prop
  method redirect_uri_ : js_string t optdef readonly_prop
end

class type google_user = object
  method getId : js_string t meth
  method isSignedIn : bool t meth
  method getHostedDomain : js_string t meth
  method getGrantedScopes : js_string t meth
  method getBasicProfile : basic_profile_js t meth
  method getAuthResponse : auth_response_js t promise t meth
  method hasGrantedScopes : js_string t -> bool t meth
  method grant : signin_options_js t -> unit meth
  method disconnect : unit meth
end

class type current_user = object
  method get : google_user t meth
  method listen : (google_user t -> unit) callback -> unit meth
end

class type is_signed_in = object
  method get : bool t meth
  method listen : (bool t -> unit) callback -> unit meth
end

class type offline_options = object
  method prompt : js_string t optdef readonly_prop
  method scope : js_string t optdef readonly_prop
end

class type google_auth = object
  method isSignedIn : is_signed_in t prop
  method signIn : signin_options_js t optdef -> google_user t promise t meth
  method signOut : unit promise t meth
  method disconnect : unit meth
  method grantOfflineAccess : offline_options t -> Unsafe.any promise t meth
  method attachClickHandler : Unsafe.any -> signin_options_js t
    -> (google_user t -> unit) callback -> (Unsafe.any -> unit) callback -> unit meth
  method currentUser : current_user t prop
end

class type init_params_js = object
  method client_id_ : js_string t readonly_prop
  method cookie_policy_ : js_string t optdef readonly_prop
  method scope : js_string t optdef readonly_prop
  method fetch_basic_profile_ : bool t optdef readonly_prop
  method hosted_domain_ : js_string t optdef readonly_prop
  method ux_mode_ : js_string t optdef readonly_prop
  method redirect_uri_ : js_string t optdef readonly_prop
end

class type authorize_config = object
  method client_id_ : js_string t readonly_prop
  method scope : js_string t readonly_prop
  method response_type_ : js_string t optdef readonly_prop
  method prompt : js_string t optdef readonly_prop
  method cookie_policy_ : js_string t optdef readonly_prop
  method hosted_domain_ : js_string t optdef readonly_prop
  method login_hint_ : js_string t optdef readonly_prop
  method include_granted_scopes_ : bool t optdef readonly_prop
end

class type authorize_response = object
  inherit auth_response_js
  method error : js_string t optdef prop
  method error_subtype_ : Unsafe.any optdef prop
end

class type auth2 = object
  method init : init_params_js t -> google_auth t promise t meth
  method getAuthInstance : google_auth t meth
  method authorize : authorize_config t -> (authorize_response t -> unit) callback -> unit meth
end

class type render_options_js = object
  method scope : js_string t optdef readonly_prop
  method width : int optdef readonly_prop
  method height : int optdef readonly_prop
  method longtitle : bool t optdef readonly_prop
  method theme : js_string t optdef readonly_prop
  method onsuccess : (google_user t -> unit) callback optdef readonly_prop
  method onfailure : (Unsafe.any -> unit) callback optdef readonly_prop
end

class type signin2 = object
  method render : Unsafe.any -> render_options_js t -> unit meth
end

class type gapi = object
  method load : js_string t -> (unit -> unit) callback -> unit meth
  method auth2 : auth2 t optdef readonly_prop
  method signin2 : signin2 t readonly_prop
end

type init_params = {
  client_id : string;
  cookie_policy : string option;
  scope : string option;
  fetch_basic_profile : bool option;
  hosted_domain : string option;
  ux_mode : string option;
  redirect_uri : string option;
}

let init_params p : init_params_js t = object%js
  val client_id_ = string p.client_id
  val cookie_policy_ = optdef string p.cookie_policy
  val scope = optdef string p.scope
  val fetch_basic_profile_ = optdef bool p.fetch_basic_profile
  val hosted_domain_ = optdef string p.hosted_domain
  val ux_mode_ = optdef string p.ux_mode
  val redirect_uri_ = optdef string p.redirect_uri
end

type sigin_options = {
  prompt : string option;
  scope_s : string option;
  ux_mode_s : string option;
  redirect_uri_s : string option
}

let signin_options o : signin_options_js t = object%js
  val prompt = optdef string o.prompt
  val scope = optdef string o.scope_s
  val ux_mode_ = optdef string o.ux_mode_s
  val redirect_uri_ = optdef string o.redirect_uri_s
end

let signin_options_opt o =
  match o with
  | None -> object%js
    val prompt = undefined
    val scope = undefined
    val ux_mode_ = undefined
    val redirect_uri_ = undefined
  end
  | Some o -> signin_options o

type render_options = {
  scope_r : string option;
  width : int option;
  height : int option;
  longtitle : bool option;
  theme : string option;
  onsuccess : (google_user t -> unit) option;
  onfailure : (Unsafe.any -> unit) option
}

let render_options o : render_options_js t = match o with
  | None -> object%js
    val scope = undefined
    val width = undefined
    val height = undefined
    val longtitle = undefined
    val theme = undefined
    val onsuccess = undefined
    val onfailure = undefined
  end
  | Some o -> object%js
    val scope = optdef string o.scope_r
    val width = Optdef.option o.width
    val height = Optdef.option o.height
    val longtitle = optdef bool o.longtitle
    val theme = optdef string o.theme
    val onsuccess = optdef wrap_callback o.onsuccess
    val onfailure = optdef wrap_callback o.onfailure
  end

type basic_profile = {
  id : string;
  name : string;
  given_name : string option;
  family_name : string option;
  image_url : string option;
  email : string;
}

type auth_response = {
  access_token : string;
  id_token : string;
  scope : string;
  expires_in : int;
  first_issued_at : int;
  expires_at : int;
}

let verbose = ref false
let set_verbose v =  verbose := v

let gapi : gapi t optdef = Unsafe.variable "gapi"

let ready ?(none=fun () -> if !verbose then log_str "cannot find gapi")
    ?(timeout=500.) (f : gapi t -> unit) =
  match Optdef.to_option (Unsafe.variable "gapi") with
  | None ->
    let cb () = Optdef.case (Unsafe.variable "gapi") none f in
    ignore @@ Dom_html.window##setTimeout (wrap_callback cb) timeout
  | Some gapi -> f gapi

let init ?timeout params f =
  let params = init_params params in
  ready ?timeout @@ fun gapi ->
  gapi##load
    (string "auth2")
    (wrap_callback @@ fun () ->
     match Optdef.to_option gapi##.auth2 with
     | None -> if !verbose then log_str "error: auth2 not loaded"
     | Some auth2 ->
       let p = auth2##init params in
       jthen p f)

let get_auth (a : auth2 t) = a##getAuthInstance

let onclick ?options ?error (gauth : google_auth t) button f =
  let error = match error with
    | None -> wrap_callback (fun _ ->
        if !verbose then log_str "error during google authentication")
    | Some e -> wrap_callback e in
  let options = signin_options_opt options in
  gauth##attachClickHandler
    (Unsafe.inject button) options (wrap_callback f) error

let render ?timeout ?options button =
  let options = render_options options in
  ready ?timeout @@ fun gapi ->
  gapi##.signin2##render (Unsafe.inject button) options

let sign_in ?options (gauth : google_auth t) f =
  let options = optdef signin_options options in
  let p = gauth##signIn options in
  jthen p f

let sign_out (gauth : google_auth t) f =
  jthen gauth##signOut f

let disconnect (gauth : google_auth t) =
  gauth##disconnect

let onauth (gauth : google_auth t) f =
  gauth##.isSignedIn##listen (
    wrap_callback @@ fun b ->
    if to_bool b then f (Some gauth##.currentUser##get)
    else f None)

let onchange (gauth : google_auth t) f =
  gauth##.currentUser##listen (wrap_callback f)

let basic_profile (user : google_user t) =
  let bs = user##getBasicProfile in {
    id = to_string bs##getId;
    name = to_string bs##getName;
    given_name = to_optdef to_string bs##getGivenName;
    family_name = to_optdef to_string bs##getFamilyName;
    image_url = to_optdef to_string bs##getImageUrl;
    email = to_string bs##getEmail
  }

let user_id (user : google_user t) = to_string user##getId

let auth_response (user : google_user t) f =
  jthen user##getAuthResponse (fun r ->
      let r = {
        access_token = to_string r##.access_token_;
        id_token = to_string r##.id_token_;
        scope = to_string r##.scope;
        expires_in = r##.expires_in_;
        first_issued_at = r##.first_issued_at_;
        expires_at = r##.expires_at_ } in
      f r)

let is_signed_in (user : google_user t) =
  to_bool user##isSignedIn

let grant (user : google_user t) options =
  let options = signin_options options in
  user##grant options

let has_granted_scopes (user : google_user t) scopes =
  user##hasGrantedScopes (string scopes)

let user_disconnect (user : google_user t) =
  user##disconnect

let wrap_auth ?timeout ?options ?error params button f =
  init ?timeout params @@ fun gauth ->
  onclick ?options ?error gauth button @@ fun user ->
  let profile = basic_profile user in
  auth_response user (fun auth -> f (profile, auth))
