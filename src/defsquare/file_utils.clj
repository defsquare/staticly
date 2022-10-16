(ns defsquare.file-utils
  (:require [clojure.java.io :as io])
  (:import [java.io File]))

(defmacro ^:private predicate [s path]
  `(if ~path
     (. ~path ~s)
     false))

;; Once you've started a JVM, that JVM's working directory is set in stone
;; and cannot be changed. This library will provide a way to simulate a
;; working directory change. `cwd` is considered to be the current working
;; directory for functions in this library. Unfortunately, this will only
;; apply to functions inside this library since we can't change the JVM's
;; actual working directory.
(def ^{:doc "Current working directory. This cannot be changed in the JVM.
             Changing this will only change the working directory for functions
             in this library."
       :dynamic true}
  *cwd* (.getCanonicalFile (io/file ".")))

;; Library functions will call this function on paths/files so that
;; we get the cwd effect on them.
(defn ^File file
  "If `path` is a period, replaces it with cwd and creates a new File object
   out of it and `paths`. Or, if the resulting File object does not constitute
   an absolute path, makes it absolutely by creating a new File object out of
   the `paths` and cwd."
  [path & paths]
  (when-let [path (apply
                   io/file (if (= path ".")
                             *cwd*
                             path)
                   paths)]
    (if (.isAbsolute ^File path)
      path
      (io/file *cwd* path))))

(defn absolute?
  "Return true if `path` is absolute."
  [path]
  (predicate isAbsolute (io/file path)))

(defn executable?
  "Return true if `path` is executable."
  [path]
  (predicate canExecute (file path)))

(defn readable?
  "Return true if `path` is readable."
  [path]
  (predicate canRead (file path)))

(defn writeable?
  "Return true if `path` is writeable."
  [path]
  (predicate canWrite (file path)))

(defn delete
  "Delete `path`."
  [path]
  (predicate delete (file path)))

(defn exists?
  "Return true if `path` exists."
  [path]
  (predicate exists (file path)))

(defn absolute
  "Return absolute file."
  [path]
  (.getAbsoluteFile (file path)))

(defn normalized
  "Return normalized (canonical) file."
  [path]
  (.getCanonicalFile (file path)))

(defn ^String base-name
  "Return the base name (final segment/file part) of a `path`.
   If optional `trim-ext` is a string and the `path` ends with that
   string, it is trimmed.
   If `trim-ext` is true, any extension is trimmed."
  ([path] (.getName (file path)))
  ([path trim-ext]
     (let [base (.getName (file path))]
       (cond (string? trim-ext) (if (.endsWith base trim-ext)
                                  (subs base 0 (- (count base) (count trim-ext)))
                                  base)
             trim-ext (let [dot (.lastIndexOf base ".")]
                        (if (pos? dot) (subs base 0 dot) base))
             :else base))))

(defn directory?
  "Return true if `path` is a directory."
  [path]
  (predicate isDirectory (file path)))

(defn split-ext
  "Returns a vector of `[name extension]`."
  [path]
  (let [base (base-name path)
        i (.lastIndexOf base ".")]
    (if (pos? i)
      [(subs base 0 i) (subs base i)]
      [base nil])))

(defn extension
  "Return the extension part of a file (without the dot \".\")"
  [path]
  (let [ext (last (split-ext path))]
    (when ext
      (subs ext 1))))

(defn name
  "Return the name part of a file."
  [path] (first (split-ext path)))

(defn parent
  "Return the parent path."
  [path]
  (.getParentFile (file path)))

(defn parents
  "Get all the parent directories of a path."
  [f]
  (when-let [parent (parent (file f))]
    (cons parent (lazy-seq (parents parent)))))

(defn parse-path "Given a file return a map with following keys: dir root base name ext, nil if file doesn't exist" [f]
  (when (and (exists? f) (not (directory? f)))
    {:dir  (.toString (parent f))
     :root (.toString (last (parents f)))
     :base (base-name f)
     :name (name f)
     :absolute (.getAbsolutePath f)
     :ext  (extension f)}))

(defn name-starts-with? [s file]
  (let [{:keys [dir root base name ext]} (parse-path file)]
    (.startsWith base s)))

(defn name-ends-with? [s file]
  (let [{:keys [dir root base name ext]} (parse-path file)]
    (.endsWith base s)))

(defn name-match? [re file]
  (let [{:keys [dir root base name ext]} (parse-path file)]
    (re-find re base)))

(defn ext? [s file]
  (let [{:keys [dir root base name ext]} (parse-path file)]
    (.endsWith ext s)))

(defn markdown? [file]
  (or (ext? "md" file) (ext? "markdown" file)))

(defn re-match-filename? [re file]
  (let [{:keys [dir root base name ext] :as all} (parse-path file)]
    (re-matches re base)))

(defn list-files
  ([dir]
   (filter (comp not directory?) (file-seq (io/file dir))))
  ([dir pred]
   (filter pred (list-files dir))))
