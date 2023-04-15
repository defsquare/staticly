(ns defsquare.safari
  (:require [clojure.java.shell :as shell]
            [clojure.tools.logging :as log]))

(defn reload-safari-tab! [s]
  (log/log (clojure.tools.logging.impl/jul-factory) 'defsquare.staticly :info nil (str  "Reload Safari tab containing \"" s "\""))
  (shell/sh "osascript" :in (str " tell application \"Safari\"
   set windowList to every window
   repeat with aWindow in windowList
      set tabList to every tab of aWindow
      repeat with atab in tabList
           if (URL of atab contains \"" s "\") then
               tell atab to do Javascript \"window.location.reload()\"
           end if
      end repeat
   end repeat
end tell ")))

(defn open! [url]
  (log/info (str "Open \"" url "\" in Safari"))
  (shell/sh "osascript" :in (str " tell application \"Safari\"
        activate
        open location \""url"\"
    end tell")))
