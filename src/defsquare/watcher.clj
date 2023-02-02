(ns defsquare.watcher
  (:require [clojure.core.async :as async])
  (:import [java.nio.file Paths FileSystems WatchEvent$Kind StandardWatchEventKinds]))

(def watcher-state (atom #{}));this atom contains a set that contains the dirs with a watcher started

(defn start-watcher! [dir build-fn]
  (println (format "Start file watcher of files in dir %s" (str dir)))
  ;(println (type dir) (str dir) (Paths/get (str dir) (into-array String [])))
  (if (not (@watcher-state (str dir)))
    (let [path      (Paths/get (str dir) (into-array String []))
          watch-svc (.newWatchService (FileSystems/getDefault))]
      (.register path watch-svc (into-array WatchEvent$Kind [StandardWatchEventKinds/ENTRY_MODIFY]))
      (async/go
        (while true
          (let [key (.take watch-svc)]
            (doseq [event (.pollEvents key)]
              (println (format "File %s changed, execute function" (.toString (.context event))) )
              (build-fn))
            (.reset key))))
      (swap! watcher-state conj (str dir))
      (println "Watcher thread started for dir" (str dir)))
    (println "Watcher thread already started for dir" (str dir))))
