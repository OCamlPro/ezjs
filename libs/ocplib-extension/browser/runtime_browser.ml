open Js_types
open Promise
include Runtime_browser_common

let getBackgroundPage f = jthen runtime##getBackgroundPage f
let openOptionsPage ?callback () =
  jthen runtime##openOptionsPage (unopt_callback callback)
let setUninstallURL ?callback s =
  jthen (runtime##setUninstallURL s) (unopt_callback callback)
let requestUpdateCheck f = jthen runtime##requestUpdateCheck f
let sendMessage ?id ?options ?callback message =
  jthen (runtime##sendMessage (option id) message (option options))
    (unopt_callback callback)
let sendNativeMessage ?callback application message =
  jthen (runtime##sendNativeMessage (string application) message)
    (unopt_callback callback)
let getPlatformInfo f =
  jthen runtime##getPlatformInfo (fun o -> f (Runtime_utils.to_platform_info o))
let getPackageDirectoryEntry f =
  jthen runtime##getPackageDirectoryEntry f
