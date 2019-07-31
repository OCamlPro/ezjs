
module Runtime = Runtime_browser
module Tabs = Tabs_browser
module Storage = Storage_browser
module Content = Declarative_content

let declarativeContent = Content.declarativeContent
let storage  = Storage.storage
let sync = Storage.sync
let tabs = Tabs.tabs
let runtime = Runtime.runtime
