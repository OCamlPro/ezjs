
module Runtime_lwt = Runtime_browser_lwt
module Tabs_lwt = Tabs_browser_lwt
module Storage_lwt = Storage_browser_lwt
module Content = Declarative_content
module Windows_lwt = Windows_browser_lwt
module I18n_lwt = I18n_browser_lwt

let declarativeContent = Content_lwt.declarativeContent
let storage  = Storage_lwt.storage
let sync = Storage_lwt.sync
let tabs = Tabs_lwt.tabs
let runtime = Runtime_lwt.runtime
let windows = Windows_lwt.windows
let i18n = I18n_lwt.i18n
