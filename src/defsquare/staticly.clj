(ns defsquare.staticly
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [clojure.tools.logging :as log]
            [clojure.java.shell :as shell]


            [hickory.core :as hickory :refer [as-hiccup parse-fragment]]
            [hiccup.core :as hiccup]

            [defsquare.markdown :as md]
            [defsquare.file-utils :as file-utils]
            [defsquare.staticly :as staticly]))

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

(defn- drop-extension [relative-path]
  (subs (str/replace relative-path #"\.\w*$" "") 1))

(defn- html-extension [relative-path]
  (let [path (str/replace relative-path #"\.\w*$" ".html")]
    (when (not (str/blank? path))
      (subs path 1))))

(defn- extension [x]
  (case (type x)
    java.lang.String (last (str/split x #"\."))
    java.io.File     (extension (.getPath x))))

(defn- file-separator
  "This function returns the platform specific file separator
   and handles some platform specific issues as they arise.
   One particular issue is that the value returned by the File
   API on windows '\\' breaks the re-pattern function."
  []
  (let [separator (java.io.File/separator)]
    ;; Windows, replace with double escape.
    (str/replace separator "\\" "\\\\")))

(defn- ensure-out-dir [out-path drop-last?]
  (let [split-path (str/split out-path (re-pattern (file-separator)))
        split-path (if drop-last? (drop-last split-path) split-path)
        intermediate-paths (map-indexed
                             (fn [i _]
                               (str/join (file-separator) (take (inc i) split-path)))
                             split-path)]
    (doseq [path intermediate-paths]
      (let [file (io/file path)]
        (when-not (.isDirectory file)
          (.mkdir file))))))

(def rendered-filetypes #{"md" "mds" "clj" "cljc" "cljs" "yaml" "json" "edn"})

(def copied-filetypes #{"jpg" "png" "svg" "css"})

(defn- dest-path [{:as params :keys [from to dest-path-fn]} path]
  (let [out-path-fn (or dest-path-fn drop-extension)
        single-file? (= path from)
        to-dir? (or (.isDirectory (io/file to))
                    (= (last (.getPath (io/file path))) (java.io.File/separatorChar)))
        relative-from-path (if single-file? path (relative-path path from))]
    ;(println dest-path-fn single-file? to-dir? relative-from-path params path)
    (if (and single-file? (not to-dir?))
      ;; then we're just translating a single file with an explicit to path
      to
      ;; then we need to assume that we're exporting to a path which has a directory created for it
      (let [dest (dest-path-fn relative-from-path)]
        (join-paths (or to ".") dest)))))

(defn copy-asset! [params file]
  (let [dest-path (dest-path (assoc params :dest-path-fn identity) (.getPath file) )]
    (ensure-out-dir dest-path true)
    (println (format "Copy file %s to %s" file dest-path))
    (io/copy file (io/file dest-path)) ))

(defn- replace-nil-with-blank [v]
  (map (fn [x] (if x x "")) v))

(defn- remove-empty-form [x]
  (if (and (string? x) (clojure.string/blank? x))
    nil
    (if (vector? x)
      (if (= 2 (count x))
        (vec (replace-nil-with-blank x))
        (vec (filter remove-empty-form x)))
      (if (and (map? x) (empty? x))
        nil
        x))))

(defn clean-hiccup [hiccup]
  (clojure.walk/postwalk remove-empty-form hiccup))

(defn html-str->hiccup [html-str]
  (->> html-str
       hickory/parse-fragment
       (map hickory/as-hiccup)
       clean-hiccup))

(defn html-file->hiccup [f]
  (html-str->hiccup (slurp f)))

(defn html->hiccup [html]
  (let [f (io/file html)
        s (if (.exists f)
            (slurp f)
            html)]
    (html-str->hiccup s)))

(defn determine-template [{:keys [single-templates] :as params} file]
  (some (fn [[re-string template]] (when (re-find (re-pattern re-string) (.getPath file))
                             template)) single-templates))

(defmulti render (fn [_ file]
                   (file-utils/extension (.getPath file))))

(defmethod render "md" [params file]
  (println (format "Render file %s" file))
  (let [template (determine-template params file)]
    (println (format "DEBUG Render file %s %s %s" file template params))
    ;(println (type template))
    (-> file
        slurp
        md/process
        template
        hiccup/html)))


;(re-find #"^blog/.*\.md$" "blog/offer.md")

(defn build-file! [{:as params :keys [format from to single-templates as-assets? compile-opts]} src-file]
  (when (and from (.isDirectory (io/file from)))
    (ensure-out-dir to false))
  (if-let [{:keys [ext]} (and src-file (file-utils/parse-path src-file))]
    (let [dest-file (dest-path params src-file)]
      (println (clojure.core/format "Build file %s with extension %s to %s" src-file ext dest-file))
      (cond
        (or as-assets? (copied-filetypes ext)) (copy-asset! params src-file)
        (rendered-filetypes ext)               (when (and src-file (not (.isDirectory src-file)))
                                                 (let [filename (.getPath src-file)
                                                       ext      (file-utils/extension filename)
                                                       contents (slurp filename)]
                                     (spit dest-file (render params src-file ))))))))

(defn build-dir! [{:as params :keys [from aggregate-templates single-templates to]} src-dir]
  (when aggregate-templates
    ;;list markdowns file except the ones starting with DRAFT.
    (let [all-markdowns-with-meta (md/list-markdowns-with-meta src-dir #"(?!DRAFT\.).*\.md")]
      (doseq [template aggregate-templates]
        (println "build-dir!" src-dir params template)
        (let [content (template all-markdowns-with-meta)
              dest-file (str to java.io.File/separator "index.html")]
          (println (clojure.core/format "Build directory %s with aggregate-template %s to %s" from (str template) to))
          (spit dest-file (hiccup/html content)))))))

(defn build!
  "Builds a static web site based on the content specified in specs. Each build-desc should be a mapping of paths, with additional
  details about how to build data from one path to the other. Available build params keys are:
    * `:from`        - (required) Path from which to build
    * `:to`          - (required) Compiled files go here
    * `:single-templates` - Map of Regex that match the file path to template as Function which takes :hiccup and :metadata and returns some new hiccup, presumably placing the content in question in some place
    * `:aggregate-templates` - vector of templates as function wich takes the return of `all-markdowns-with-meta`

    * `:out-path-fn` - Function used for naming compilation output
    * `:to-format`   - Literal format to use for export!
    * `:to-format-fn` - Function of input filename to format
    * `:as-assets?` - Pass through as a static assets (for images, css, json or edn data, etc)
      - Note: by default, images, css, etc will pass through anyway
  Additional options pertinent to the entire build process may be passed in:
    * `:lazy?` - If true, don't build anything until it changes; this is best for interactive/incremental updates and focused work.
                 Set to false if you want to rebuild from scratch. (default true)
    * `:root-dir` - Static assets will be served relative to this directory (defaults to greatest-common-path between all paths)
  "
  [params]
  (let [{:keys [from] :as params} (merge {:dest-path-fn html-extension} params)]
    (build-dir! params from)
    (doseq [src-file (file-seq (io/file from))]
      (build-file! params src-file))))


(defn reload-safari-tab! [s]
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

(def watcher-state (atom :stopped))

(defn start-watcher! [dir build-fn]
  (if (= :stopped @watcher-state)
    (let [path      (java.nio.file.Paths/get dir (into-array java.lang.String []))
          watch-svc (.newWatchService (java.nio.file.FileSystems/getDefault))]
      (.register path watch-svc (into-array java.nio.file.WatchEvent$Kind [java.nio.file.StandardWatchEventKinds/ENTRY_MODIFY]))
      (println (format "Start file watcher of files in dir %s" dir))
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

(defn ns-last-name [ns]
  (subs (str ns) (inc (clojure.string/last-index-of (str ns) "."))))

(defn ns-first-name [ns]
  (subs (str ns) 0 (.indexOf (str ns) ".")))

(def BUILD_FN_NAME "build!")
(def EXPORT_FN_NAME "export!")
(def PUBLIC_DIR "resources/public/")

(defmacro emit-export [to]
  `(defn ~(symbol EXPORT_FN_NAME)
         ([hiccup#]
          (~(symbol EXPORT_FN_NAME) hiccup# ~to))
     ([hiccup# filename#]
          (println "Export html to" filename#)
          (spit filename# (hiccup/html hiccup#)))))

(defmacro emit-build [reload-word render-fn]
  `(defn ~(symbol BUILD_FN_NAME) []
     ;;invoke export
     (~(symbol EXPORT_FN_NAME) (~(symbol (str *ns*) render-fn)))
     (when (not= "CLOUDFLARE" (environ.core/env :build-context))
       (staticly/reload-safari-tab! ~reload-word))))

(defmacro emit-main [render-fn]
  `(defn ~(symbol "-main") [& args#]
     (~(symbol EXPORT_FN_NAME) (~ (symbol (str *ns*) render-fn)))
     (shutdown-agents)))

(defmacro emit-dev-build []
  `(when (not= "CLOUDFLARE" (environ.core/env :build-context))
     (~(symbol (str *ns*) BUILD_FN_NAME))))

(defn execution-context []
  {:project-name (ns-first-name *ns*)
   :doc-name     (ns-last-name *ns*)})

(defmacro def-render-builder
  ([]
   (let [{:keys [project-name doc-name]} (execution-context)
         export-dir                      PUBLIC_DIR
         export-file                     (str export-dir doc-name ".html")]
     `(defbuilder {:to          ~export-file
                   :render-fn   "render"
                   :reload-word ~project-name})))
  ([{:keys [to render-fn reload-word] :as params}]
   `(do
      (println (format "Def Staticly builder: rendering function \"%s\" exporting HTML to %s" ~render-fn ~to))
      (emit-export ~to)
      (emit-build ~reload-word ~render-fn)
      (emit-main ~render-fn)
      (emit-dev-build)
      nil)))

(defmacro emit-md-build [params]
  (let []
    `(defn ~(symbol BUILD_FN_NAME) []
       (staticly/build! ~params)
       (when (not= "CLOUDFLARE" (environ.core/env :build-context))
         (reload-safari-tab! ~(:reload-word params))
         (println (format "Reload Safari tab containing \"" ~(:reload-word params) "\""))))))

(defmacro def-blog-builder
  ([]
   (let [{:keys [project-name doc-name]} (execution-context)]
     `(def-blog-builder {:from                ~doc-name
                         :to                  (str PUBLIC_DIR ~doc-name)
                         :single-templates    {(str "^.*" ~doc-name "/.*\\.md$") ~(symbol "post-template")}
                         :aggregate-templates [(symbol "home-template")]
                         :reload-word          ~project-name})))
  ([{:keys [from to single-templates aggregate-templates] :as params}]
   `(do
      (println (format "Define Staticly Blog builder: markdowns in %s dir rendered using single-template %s and aggregate-template %s exported to %s" ~from ~single-templates ~aggregate-templates ~to))
      (emit-md-build ~params)
      (when (not= "CLOUDFLARE" (environ.core/env :build-context))
        (~(symbol (str *ns*) BUILD_FN_NAME))
        (staticly/start-watcher! ~from ~(symbol (str *ns*) BUILD_FN_NAME))))))

(defmacro emit-page-build [params]
  `(defn ~(symbol BUILD_FN_NAME) []
     (staticly/build! ~params)
     (when (not= "CLOUDFLARE" (environ.core/env :build-context))
       (staticly/reload-safari-tab! ~(:reload-word params))
       (println (format "Reload safari tab containing \"defsquare\"")))))

(defmacro def-page-builder
  ([]
   (let [{:keys [project-name doc-name]} (execution-context)]
     `(def-page-builder {:from                ~doc-name
                         :to                  ~PUBLIC_DIR
                         :single-templates    {(str "^.*" ~doc-name "/.*\\.md$") ~(symbol "page-template")}
                         :reload-word          ~project-name})))
  ([{:keys [from to single-templates] :as params}]
   `(do
      (require 'environ.core)
      (println (format "Define Staticly builder for %s: markdowns in \"%s\" dir rendered using single-templates mapping %s exported to dir \"%s\"" ~(str *ns*) ~from ~single-templates ~to))
      (emit-page-build ~params)
      (when (not= "CLOUDFLARE" (environ.core/env :build-context))
        (~(symbol (str *ns*) BUILD_FN_NAME))
        (staticly/start-watcher! ~from ~(symbol (str *ns*) BUILD_FN_NAME))))))
