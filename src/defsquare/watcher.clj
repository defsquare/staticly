(ns defsquare.watcher
  (:require [clojure.core.async :as async])
  (:import [java.nio.file Paths FileSystems WatchEvent$Kind StandardWatchEventKinds]))

(def watcher-state (atom :stopped))

(defn start-watcher! [dir build-fn]
  (println (format "Start file watcher of files in dir %s" dir))
  (if (= :stopped @watcher-state)
    (let [path      (Paths/get dir (into-array String []))
          watch-svc (.newWatchService (FileSystems/getDefault))]
      (.register path watch-svc (into-array WatchEvent$Kind [StandardWatchEventKinds/ENTRY_MODIFY]))
      (async/go
        (while true
          (let [key (.take watch-svc)]
            (doseq [event (.pollEvents key)]
              (println (format "File %s changed, build" (.toString (.context event))) )
              (build-fn))
            (.reset key))))
      (reset! watcher-state :started)
      (println "Watcher thread started"))
    (println "Watcher thread already started")))
