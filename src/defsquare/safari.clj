(ns defsquare.safari
  (:require [clojure.java.shell :as shell]
            [clojure.tools.logging :as log]))

(defn reload-tab! [s]
  (log/info (str  "Reload Safari tab containing \"" s "\""))
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

(defn open-or-reload! [url]
  (log/info (str "Open or reload \"" url "\" in Safari"))
  (shell/sh "osascript" :in (str " set targetURL to \"" url "\"
    tell application \"Safari\"
        activate
        set windowList to every window
        set tabFound to false

        repeat with aWindow in windowList
            set tabList to every tab of aWindow
            repeat with aTab in tabList
                if (URL of aTab contains targetURL) then
                    set tabFound to true
                    tell aTab to do JavaScript \"window.location.reload()\"
                    exit repeat
                end if
            end repeat
            if tabFound then
                exit repeat
            end if
        end repeat

        if not tabFound then
            open location targetURL
        end if
    end tell ")))
