(ns defsquare.watcher
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io]
            [defsquare.file-utils :as file-utils])
  (:import [java.nio.file Paths FileSystems WatchEvent$Kind StandardWatchEventKinds]))

(def watcher-dir-state (atom #{}));this atom contains a set that contains the dirs with a watcher started
(def watcher-file-state (atom #{}));this atom contains a set that contains the files with a watcher started

(defn watcher-started? [dir]
  )

(defn start-watcher! [dir build-fn]
  (let [[dir file] (when (.isFile (io/file dir)) [(file-utils/parent dir) dir])]
    (if file
        (println (format "Start file watcher for file %s" (str file)))
        (println (format "Start file watcher of files in dir %s" (str dir))))
    (if (not (@watcher-dir-state (str dir)))
      (let [dir-path  (Paths/get (str dir) (into-array String []))
            file-path (when file (Paths/get (str file) (into-array String [])))
            watch-svc (.newWatchService (FileSystems/getDefault))]
        (.register dir-path watch-svc (into-array WatchEvent$Kind [StandardWatchEventKinds/ENTRY_MODIFY]))
        (async/go
          (while true
            (let [key (.take watch-svc)]
              (doseq [event (.pollEvents key)]
                (let [path-changed (.context event)]
                 ; (if (.endsWith ))
                  )
                (println (format "File %s changed, execute function" (.toString (.context event))) )
                (build-fn))
              (.reset key))))
        (swap! watcher-state conj (str dir))
        (println "Watcher thread started for dir" (str dir)))
      (println "Watcher thread already started for dir" (str dir)))))

(comment
  (defn watch-service [path-str]
    (let [path          (clojure.java.io/file path-str)
          watch-service (java.nio.file.FileSystem/getDefault)
          .newWatchService
          watch-key     (.register path watch-service java.nio.file/StandardWatchEventKinds/ENTRY_MODIFY)]
      (while true
        (let [wk     (.take watch-service)
              events (.pollEvents wk)]
          (doseq [event events]
            (let [changed (-> event .context (cast java.nio.file.Path))]
              (println changed)
              (when (.endsWith changed "myFile.txt")
                (println "My file has changed"))))
          (let [valid (.reset wk)]
            (when-not valid
              (println "Key has been unregistered")))))
      (println (clojure.java.io/file path-str)))))
