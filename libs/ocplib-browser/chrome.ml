
module Runtime = Runtime_chrome
module Tabs = Tabs_chrome
module Storage = Storage_chrome
module Content = Declarative_content
module Windows = Windows_chrome
module I18n = I18n_chrome

let runtime = Runtime.runtime
let declarativeContent = Content.declarativeContent
let storage = Storage.storage
let sync = Storage.sync
let tabs = Tabs.tabs
let windows = Windows.windows
let i18n = I18n.i18n
