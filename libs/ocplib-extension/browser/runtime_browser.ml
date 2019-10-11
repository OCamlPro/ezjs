open Js_types
open Promise
open Browser_utils
open Runtime_utils
module Dom_html = Js_of_ocaml.Dom_html

class type onInstalledEvent = object
  method id : js_string t optdef prop
  method previousVersion : js_string t optdef prop
  method reason : js_string t prop
  method temporary : bool t prop
end

class type runtime = object
  method lastError : error t prop
  method id : js_string t prop
  method getBackgroundPage : Dom_html.window t promise t meth
  method openOptionsPage : unit promise t meth
  method getManifest : 'a t meth
  method getURL : js_string t -> js_string t meth
  method setUninstallURL : js_string t -> unit promise t meth
  method reload : unit meth
  method requestUpdateCheck : requestCheckResponse t prop promise t meth
  method connect : js_string t opt -> 'a t opt -> port t meth
  method sendMessage : js_string t opt -> 'a t -> connectInfo t opt -> 'b t promise t meth
  method sendNativeMessage : js_string t -> 'a t -> 'b t promise t meth
  method getPlatformInfo : platformInfo t promise t meth
  method getBrowserInfo : browserInfo t promise t meth
  method getPackageDirectoryEntry : 'a t meth
  method onStartup : unit event t prop
  method onInstalled : onInstalledEvent t event t prop
  method onSuspend : unit event t prop
  method onSuspendCanceled : unit event t prop
  method onUpdateAvailable : 'a t event t prop
  method onBrowserUpdateAvailable : unit event t prop
  method onConnect : port t event t prop
  method onConnectExternal : port t event t prop
  method onMessage : ('a t,  js_string t, ('a t -> bool t) callback) event3 t prop
  method onMessageExternal : ('a t,  js_string t, ('a t -> bool t) callback) event3 t prop
  method onRestartRequired : js_string t event t prop
end

let runtime : runtime t = variable "browser.runtime"

let last_error () = runtime##.lastError
let id () = runtime##.id
let getBackgroundPage f = jthen runtime##getBackgroundPage f
let openOptionsPage ?callback () =
  jthen runtime##openOptionsPage (unopt_callback callback)
let getManifest () = runtime##getManifest
let gerURL s = to_string @@ runtime##getURL (string s)
let setUninstallURL ?callback s =
  jthen (runtime##setUninstallURL s) (unopt_callback callback)
let reload () = runtime##reload
let requestUpdateCheck f = jthen runtime##requestUpdateCheck f
let connect ?id ?info () =
  let id = option id in
  let info = option info in
  runtime##connect id info
let sendMessage ?id ?options ?callback message =
  jthen (runtime##sendMessage (option id) message (option options))
    (unopt_callback callback)
let sendNativeMessage ?callback application message =
  jthen (runtime##sendNativeMessage (string application) message)
    (unopt_callback callback)
let getPlatformInfo f =
  jthen runtime##getPlatformInfo (fun o -> f (to_platform_info o))
let getPackageDirectoryEntry f =
  jthen runtime##getPackageDirectoryEntry f

let onStartup f = addListener1 runtime##.onStartup f
let onInstalled f = addListener1 runtime##.onInstalled f
let onSuspend f = addListener1 runtime##.onSuspend f
let onSuspendCanceled f = addListener1 runtime##.onSuspendCanceled f
let onUpdateAvailabale f = addListener1 runtime##.onUpdateAvailable f
let onConnect f = addListener1 runtime##.onConnect f
let onConnectExternal f = addListener1 runtime##.onConnectExternal f
let onMessage f = addListener3 runtime##.onMessage f
let onMessageExternal f = addListener3 runtime##.onMessageExternal f
let onRestartRequired f = addListener1 runtime##.onRestartRequired f
