
module Runtime_lwt = Runtime_chrome_lwt
module Tabs_lwt = Tabs_chrome_lwt
module Storage_lwt = Storage_chrome_lwt
module Content_lwt = Declarative_content_lwt
module Windows_lwt = Windows_chrome_lwt
module I18n_lwt = I18n_chrome_lwt

let runtime = Runtime_lwt.runtime
let declarativeContent = Content_lwt.declarativeContent
let storage = Storage_lwt.storage
let sync = Storage_lwt.sync
let tabs = Tabs_lwt.tabs
let windows = Windows_lwt.windows
let i18n = I18n_lwt.i18n
