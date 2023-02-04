(ns defsquare.watcher-test
  (:require [clojure.test :as test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [defsquare.watcher :as watcher]))

(defn temp-dir [dir-name]
  (java.nio.file.Files/createTempDirectory
   (.toPath (io/as-file (System/getProperty "java.io.tmpdir")))
   dir-name
   (into-array java.nio.file.attribute.FileAttribute [])))

(defn sh [command]
  (assert shell/*sh-dir* "Can't run commands without a specified directory.")
  (let [result (shell/sh "/bin/bash" "-c" command)]
     (assert (-> result :exit zero?) (:err result))
     result))

(defn mkdir-p!
  "create a bunch of dirs all at the same time"
  [& dirs]
  (when dirs
    (sh (str "mkdir -p " (apply str (interpose "/" dirs))))))

(defn temp-name
  "Create a temporary file name like what is created for [[temp-file]]
   and [[temp-dir]]."
  ([prefix] (temp-name prefix ""))
  ([prefix suffix]
     (format "%s%s-%s%s" prefix (System/currentTimeMillis)
             (long (rand 0x100000000)) suffix)))

(defn dummy-filename []
  (temp-name "dummy" ".txt"))

(defn write-dummy-file-in! [filename & dirs]
  ;(apply mkdir-p! dirs)
  (let [path-str (str (apply str (interpose "/" dirs)) (when dirs "/") filename)
        path-dir (java.nio.file.Paths/get (apply str (interpose "/" dirs)) (into-array String []))
        path     (java.nio.file.Paths/get path-str (into-array String []))]
    ;(println (.toString path-dir) path-str (.toString path))
    (.mkdirs (.toFile path-dir))
    (spit (.toString (.toAbsolutePath path)) (str (java.util.UUID/randomUUID)))
    (.toFile path)))

(deftest watcher
  (testing "watch a dir"
    (let [dir (temp-dir "dir-to-watch")])
    )
  (testing "watch a file"))
