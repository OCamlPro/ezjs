
module Runtime = Runtime_chrome
module Tabs = Tabs_chrome
module Storage = Storage_chrome
module Content = Declarative_content

let runtime = Runtime.runtime
let declarativeContent = Content.declarativeContent
let storage = Storage.storage
let sync = Storage.sync
let tabs = Tabs.tabs
