open Js_types
open Runtime_utils
include Runtime_chrome_common

let getBackgroundPage f = runtime##getBackgroundPage (wrap_callback f)
let openOptionsPage ?callback () = runtime##openOptionsPage (optdef_wrap callback)
let setUninstallURL ?callback s = runtime##setUninstallURL s (optdef_wrap callback)
let requestUpdateCheck f = runtime##requestUpdateCheck (wrap_callback f)
let restartAfterDelay ?callback i = runtime##restartAfterDelay i (optdef_wrap callback)
let sendMessage ?id ?options ?callback message =
  runtime##sendMessage (def_option id) message (def_option options)
    (def_option callback)
let sendNativeMessage ?callback application message =
  runtime##sendNativeMessage (string application) message (def_option callback)
let getPlatformInfo f =
  runtime##getPlatformInfo (wrap_callback (fun o -> f (to_platform_info o)))
let getPackageDirectoryEntry f =
  runtime##getPackageDirectoryEntry (wrap_callback f)
