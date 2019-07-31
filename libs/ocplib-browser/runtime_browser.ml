open Js_min
open Js
open Browser_utils
open Runtime_utils

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
  method getManifest : json t meth
  method getURL : js_string t -> js_string t meth
  method setUninstallURL : js_string t -> unit promise t meth
  method reload : unit meth
  method requestUpdateCheck : requestCheckResponse t prop promise t meth
  method connect : js_string t opt -> 'a t opt -> port t meth
  method sendMessage : js_string t opt -> 'a t -> 'b t opt -> json t promise t meth
  method sendNativeMessage : js_string t -> 'a t -> json t promise t meth
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

let runtime : runtime t = Unsafe.variable "browser.runtime"
let onStartup : unit event t = runtime##onStartup
let onInstalled : onInstalledEvent t event t = runtime##onInstalled
let onSuspend : unit event t = runtime##onSuspend
let onSuspendCanceled : unit event t = runtime##onSuspendCanceled
let onConnect : port t event t = runtime##onConnect
let onConnectExternal : port t event t = runtime##onConnectExternal
let onRestartRequired : js_string t event t = runtime##onRestartRequired

let last_error () = runtime##lastError
let id () = runtime##id
let getBackgroundPage f = jthen runtime##getBackgroundPage() f
let openOptionsPage ?callback () =
  jthen runtime##openOptionsPage() (unopt_callback callback)
let getManifest () = runtime##getManifest()
let gerURL s = to_string runtime##getURL(string s)
let setUninstallURL ?callback s =
  jthen runtime##setUninstallURL(s) (unopt_callback callback)
let reload () = runtime##reload()
let requestUpdateCheck f = jthen runtime##requestUpdateCheck() f
let connect ?id ?info () =
  let id = Opt.option id in
  let info = Opt.option info in
  runtime##connect(id, info)
let sendMessage ?id ?options ?callback message =
  jthen runtime##sendMessage(Opt.option id, message, Opt.option options)
    (unopt_callback callback)
let sendNativeMessage ?callback application message =
  jthen runtime##sendNativeMessage(string application, message)
    (unopt_callback callback)
let getPlatformInfo f =
  jthen runtime##getPlatformInfo() (fun o -> f (to_platform_info o))
let getPackageDirectoryEntry f =
  jthen runtime##getPackageDirectoryEntry() f
