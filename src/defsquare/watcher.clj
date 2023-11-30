(ns defsquare.watcher
  (:require [clojure.core.async :as async]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [defsquare.files :as files])
  (:import [java.nio.file Paths FileSystems WatchEvent$Kind StandardWatchEventKinds]))

(defonce watcher-dir-state (atom #{}));this atom contains a set that contains the dirs with a watcher started
(defonce watcher-file-state (atom #{}));this atom contains a set that contains the files with a watcher started
(defonce watch-keys (atom #{}))

(defn reset-watcher! []
  (doseq [watch-key @watch-keys]
    (.cancel watch-key))
  (reset! watch-keys #{})
  (reset! watcher-dir-state #{})
  (reset! watcher-file-state #{}))

(defn watcher-for-dir-started? [dir-path]
  (@watcher-dir-state (str dir-path)))

(defn watcher-for-file-started? [file-path]
  (@watcher-file-state (str file-path)))

(defn- start-watch-service [dir-path file-path watcher-for-dir? watcher-for-file? fn-to-execute]
  (let [watch-svc (.newWatchService (FileSystems/getDefault))
        watch-key (.register dir-path watch-svc (into-array WatchEvent$Kind [StandardWatchEventKinds/ENTRY_MODIFY StandardWatchEventKinds/ENTRY_CREATE]))]
    (swap! watch-keys conj watch-key)
    (async/go
      (while true
        (let [key (.take watch-svc)]
          (doseq [event (.pollEvents key)]
            (let [path-changed (.context event)
                  absolute-path-changed (.toAbsolutePath (Paths/get (str dir-path) (into-array String [(str path-changed)])))]
                                        ;(println "path changed" (str absolute-path-changed) (str file-path) watcher-for-file? watcher-for-dir?)
              (when (and watcher-for-file? (.endsWith (str absolute-path-changed) (str file-path)))
                (log/infof "File %s in dir %s changed, execute function" file-path (.toString (.context event)) )
                (fn-to-execute)))
            (when watcher-for-dir?
              (log/infof "Dir \"%s\": File %s changed, execute function" dir-path (.toString (.context event)) )
              (fn-to-execute))
            (.reset key)))))
    watch-svc))

(defn start-watcher! [file-or-dir fn-to-execute]
  (let [watcher-for-file? (.isFile (io/file file-or-dir))
        watcher-for-dir?  (not watcher-for-file?)
        [dir file] (if watcher-for-file? [(files/parent file-or-dir) file-or-dir] [file-or-dir nil])
        dir-path (Paths/get (str dir) (into-array String []))
        file-path (when file (Paths/get (str file) (into-array String [])))]
    (if watcher-for-file?
      (log/infof "Start file watcher for file %s" (str file))
      (log/infof "Start file watcher of files in dir \"%s\"" (str dir)))
    (if (not (or (watcher-for-dir-started? dir-path)
                 (watcher-for-file-started? file-path)))
      (do
        (let [watcher (start-watch-service dir-path file-path watcher-for-dir? watcher-for-file? fn-to-execute)]
          (log/info "Watcher thread started for" (if watcher-for-file? (format "file %s" (str file-path)) (format "dir %s" (str dir-path))))
          (if watcher-for-file?
            (swap! watcher-file-state conj (str file-path))
            (swap! watcher-dir-state conj (str dir-path)))
          watcher))
      (log/info "Watcher thread already started for" (if watcher-for-file? (format "file %s" (str file-path)) (format "dir %s" (str dir-path)))))))

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
