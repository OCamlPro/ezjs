open Js_types
open Promise
open Browser_utils_lwt
open Runtime_utils_lwt


class type onInstalledEvent = object
  method id : js_string t optdef prop
  method previousVersion : js_string t optdef prop
  method reason : js_string t prop
end

class type runtime = object
  method lastError : error t prop
  method id : js_string t prop
  method getBackgroundPage : (window t -> unit) callback -> unit meth
  method openOptionsPage : (unit -> unit) callback optdef -> unit meth
  method getManifest : 'a t meth
  method getURL : js_string t -> js_string t meth
  method setUninstallURL : js_string t -> (unit -> unit) callback optdef -> unit meth
  method reload : unit meth
  method requestUpdateCheck : (requestCheckResponse t -> unit) callback -> unit meth
  method restart : unit meth
  method restartAfterDelay : int -> (unit -> unit) callback optdef -> unit meth
  method connect : js_string t optdef -> connectInfo t optdef -> port t meth
  method connectNative : js_string t -> port t meth
  method sendMessage : js_string t optdef -> 'a t -> connectInfo t optdef -> ('b t -> unit) callback optdef -> unit meth
  method sendNativeMessage : js_string t -> 'a t -> ('b t -> unit) callback optdef -> unit meth
  method getPlatformInfo : (platformInfo t -> unit) callback -> unit meth
  method getPackageDirectoryEntry : ('a t -> unit) callback -> unit meth
  method onStartup : unit event t prop
  method onInstalled : onInstalledEvent t event t prop
  method onSuspend : unit event t prop
  method onSuspendCanceled : unit event t prop
  method onUpdateAvailable : 'a t event t prop
  method onConnect : port t event t prop
  method onConnectExternal : port t event t prop
  method onConnectNative : port t event t prop
  method onMessage : ('a t,  messageSender t, ('a t -> bool t) callback) event3 t prop
  method onMessageExternal : ('a t,  messageSender t, ('a t -> bool t) callback) event3 t prop
  method onRestartRequired : js_string t event t prop
end

let runtime : runtime t = variable "chrome.runtime"

let last_error () = runtime##.lastError
let id () = runtime##.id

let getBackgroundPage () =
  to_lwt_cb (fun cb -> runtime##getBackgroundPage cb)
let openOptionsPage ?callback () =
  to_lwt_cb_opt callback (fun cb -> runtime##openOptionsPage cb)
let getManifest () = runtime##getManifest
let getURL s = to_string @@ runtime##getURL (string s)
let setUninstallURL ?callback s =
  to_lwt_cb_opt callback (fun cb -> runtime##setUninstallURL s cb)
let reload () = runtime##reload
let requestUpdateCheck () =
  to_lwt_cb (fun cb -> runtime##requestUpdateCheck cb)
let restart () = runtime##restart
let restartAfterDelay ?callback i =
  to_lwt_cb_opt callback (fun cb -> runtime##restartAfterDelay i cb)
let connect ?id ?info () =
  runtime##connect (def_option id) (def_option info)
let connectNative application = runtime##connectNative(string application)
let sendMessage ?id ?options ?callback message =
  to_lwt_cb_opt callback (fun cb ->
      runtime##sendMessage (def_option id) message (def_option options) cb)
let sendNativeMessage ?callback application message =
  to_lwt_cb_opt callback (fun cb ->
      runtime##sendNativeMessage (string application) message cb)
let getPlatformInfo () =
  to_lwt_cb_tr to_platform_info (fun cb -> runtime##getPlatformInfo cb)
let getPackageDirectoryEntry () =
  to_lwt_cb (fun cb -> runtime##getPackageDirectoryEntry cb)

let onStartup handler =
  runtime##.onStartup##addListener (wrap_callback handler)
let onInstalled handler =
  runtime##.onInstalled##addListener (wrap_callback handler)
let onSuspend handler =
  runtime##.onSuspend##addListener (wrap_callback handler)
let onSuspendCanceled handler =
  runtime##.onSuspendCanceled##addListener (wrap_callback handler)
let onUpdateAvailabale handler =
  runtime##.onUpdateAvailable##addListener (wrap_callback handler)
let onConnect handler =
  runtime##.onConnect##addListener (wrap_callback handler)
let onConnectExternal handler =
  runtime##.onConnectExternal##addListener (wrap_callback handler)
let onConnectNative handler =
  runtime##.onConnectNative##addListener (wrap_callback handler)
let onMessage handler =
  runtime##.onMessage##addListener (wrap_callback handler)
let onMessageExternal handler =
  runtime##.onMessageExternal##addListener (wrap_callback handler)
let onRestartRequired handler =
  runtime##.onRestartRequired##addListener (wrap_callback handler)
