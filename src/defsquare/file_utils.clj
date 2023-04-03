(ns defsquare.file-utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URI]
           [java.nio.file Files Path]
           [java.nio.file.attribute  FileAttribute  PosixFilePermissions]
           [java.io File])
  (:refer-clojure :exclude [name parents]))

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
  (predicate exists (io/as-file path)))

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
  "Return true if `f` is a directory."
  [f]
  (predicate isDirectory (io/as-file f)))

(defn file?
  "Return true if `f` is a directory."
  [f]
  (predicate isFile (io/as-file f)))


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

(defn parse-path
  "Given a file return a map with following keys: dir root base name ext, nil if file doesn't exist.
  Ex.: /tmp/"
  [f]
  (if-let [f (if (string? f) (file f) f)]
    (when (and (exists? f) (not (directory? f)))
      {:dir      (.toString (parent f))
       :root     (.toString (last (parents f)))
       :base     (base-name f)
       :name     (name f)
       :absolute (.getAbsolutePath f)
       :ext      (extension f)})))

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
  (when file
    (let [{:keys [dir root base name ext] :as all} (parse-path file)]
      (re-matches re base))))

(defn list-files-in-dirs
  "list all the files (including subdir) in the directory"
  [& dirs]
  (->> dirs
      (map (comp file-seq io/file))
      (apply concat)
      (filter (comp not directory?))))

(defn list-files
  "return a list of [files](https://docs.oracle.com/javase/8/docs/api/java/io/File.html) from a unique dir or a seq of dirs. A predicate can be added to include only the files (predicate is given the file object).
  The 3-arity version's arguments are: a single dir or seq of dir, a sequence of regex that include the files if they match the file's relative path , a sequence of regex that exclude the files if they match the file's path"
  ([dirs]
   (->> (if (coll? dirs)
          (apply concat (map (comp file-seq io/file) dirs))
          (file-seq (io/file dirs)))
        (filter (comp not directory?))))
  ([dirs include?]
   (filter include? (list-files dirs)))
  ([dirs include-regexes exclude-regexes]
   (let [all-files  (list-files dirs)
        include-filter (if (empty? include-regexes)
                         (fn [_] true)
                         (fn [f] (some #(re-find % (.getPath f)) include-regexes)))
        exclude-filter (if (empty? exclude-regexes)
                         (fn [_] true)
                         (fn [f] (not (some #(re-find % (.getPath f)) exclude-regexes))))]
    (filter #(and (include-filter %) (exclude-filter %)) all-files))))


(defn as-path
  ^Path [x]
  (if (instance? Path x) x
      (if (instance? URI x)
        (java.nio.file.Paths/get ^URI x)
        (.toPath (io/file x)))))

(defn str->posix
  "Converts a string to a set of PosixFilePermission."
  [s]
  (PosixFilePermissions/fromString s))

(defn- ->posix-file-permissions [s]
  (cond (string? s) (str->posix s)
        :else s))

(defn- posix->file-attribute [x]
  (PosixFilePermissions/asFileAttribute x))

(defn- posix->attrs
  ^"[Ljava.nio.file.attribute.FileAttribute;" [posix-file-permissions]
  (let [attrs (if posix-file-permissions
                (-> posix-file-permissions
                    (->posix-file-permissions)
                    (posix->file-attribute)
                    vector)
                [])]
    (into-array FileAttribute attrs)))

(defn create-dirs!
  "Creates directories using `Files#createDirectories`. Also creates parents if needed.
  Doesn't throw an exception if the the dirs exist already. Similar to mkdir -p"
  ([path] (create-dirs! path nil))
  ([path {:keys [:posix-file-permissions]}]
   (Files/createDirectories (as-path path) (posix->attrs posix-file-permissions))))

(defn canonical-path [path-or-file]
  (-> (io/file path-or-file)
      (.getCanonicalFile)
      (.getPath)))

(defn relative-path [path base]
  (str/replace (canonical-path path)
               (canonical-path base)
               ""))

(defn strip-path-seps [path]
  (if (= (last path)
         (java.io.File/separatorChar))
    (strip-path-seps (apply str (drop-last path)))
    path))

(defn join-paths [path1 path2]
  (str (strip-path-seps path1)
       (java.io.File/separatorChar)
       path2))

(defn drop-extension [relative-path]
  (subs (str/replace relative-path #"\.\w*$" "") 1))

(defn html-extension [relative-path]
  (let [path (str/replace relative-path #"\.\w*$" ".html")]
    (when (not (str/blank? path))
      (subs path 1))))

(defn extension [x]
  (cond
    (instance? java.lang.String x) (last (str/split x #"\."))
    (instance? java.io.File x)     (extension (.getPath x))))

(defn file-separator
  "This function returns the platform specific file separator
   and handles some platform specific issues as they arise.
   One particular issue is that the value returned by the File
   API on windows '\\' breaks the re-pattern function."
  []
  (let [separator (java.io.File/separator)]
    ;; Windows, replace with double escape.
    (str/replace separator "\\" "\\\\")))

(defn ensure-out-dir
  ([out-path]
   (ensure-out-dir out-path false))
  ([out-path drop-last?]
   (let [split-path         (str/split out-path (re-pattern (file-separator)))
         split-path         (if drop-last? (drop-last split-path) split-path)
         intermediate-paths (map-indexed
                             (fn [i _]
                               (str/join (file-separator) (take (inc i) split-path)))
                             split-path)]
     (doseq [path intermediate-paths]
       (let [file (io/file path)]
         (when-not (.isDirectory file)
           (.mkdir file)))))))

(defn ns-to-source-file
  "Converts the namespace object to a source (.clj) file path."
  [ns]
  (when-let [name (try (-> ns ns-name str) (catch Exception e))]
    (let [tokens (.split name "\\.")]
      (str (apply str (interpose File/separator (map munge tokens))) ".clj"))))

(defn find-uncle-file
  "Finds an ancestor directory of file f containing a file uncle."
  [f uncle]
  (let [f (if (string? f) (File. f) f)
        uncle (if (string? uncle) uncle (.getPath uncle))
        d0 (if (.isDirectory f) f (.getParentFile f))]
    (loop [dir d0]
      (when dir
        (if (.exists (File. dir uncle))
          (.getAbsolutePath dir)
          (recur (.getParentFile dir)))))))

(defn find-resource
  [file]
  (loop [cl (.. Thread currentThread getContextClassLoader)]
    (when cl
      (println "cl" cl)
      (if-let [url (.findResource cl file)]
        url
        (recur (.getParent cl))))))

(defn project-dir
  "Returns the absolute file path of the parent of the src directory
   enclosing the current source file (or namespace's) package dirs.
   If running from a jar, returns the enclosing directory."
  ([file]
    (when-let [url (find-resource file)]
      (let [stub (.replace (.getFile url) file "")]
        (->
          (if (.endsWith stub ".jar!/")
            (.substring stub 5 (- (.length stub) 2))
            stub)
          File. .getParentFile .getAbsolutePath))))
  ([]
    (or (project-dir *file*)
        (project-dir (ns-to-source-file *ns*))
        (find-uncle-file (File. ".") "project.clj")
        (find-uncle-file (File. ".") "deps.edn"))))

(defn tmp-dir [name]
  (Files/createTempDirectory
   (.toPath (io/as-file (System/getProperty "java.io.tmpdir")))
   name
   (into-array java.nio.file.attribute.FileAttribute [])))

(defn empty? [f]
  (= 0 (count (slurp f))))
