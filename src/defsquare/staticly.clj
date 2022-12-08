(ns defsquare.staticly
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [clojure.tools.logging :as log]
            [clojure.java.shell :as shell]

            [hiccup2.core :as hiccup]
            [defsquare.markdown :as md]
        ;    [defsquare.hiccup :as hiccup]
            [defsquare.file-utils :as file-utils :refer [canonical-path relative-path strip-path-seps join-paths drop-extension html-extension extension file-separator ensure-out-dir]]
            [defsquare.rss :refer [export-rss!]])

  (:import [java.nio.file Paths FileSystems WatchEvent$Kind StandardWatchEventKinds]
           [java.io File])
  )


(def rendered-filetypes #{"md" "mds" "clj" "cljc" "cljs" "yaml" "json" "edn"})

(def copied-filetypes #{"jpg" "png" "svg" "css"})

(defn- dest-path [{:as params :keys [from to dest-path-fn]} path]
;  (println "dest-path" params path)
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

(defn dest-url [{:as params :keys [from baseurl dest-path-fn]} path]
  (str baseurl "/" (dest-path-fn (if (= path from) path (relative-path path from)))))

(defn copy-asset! [params file]
  (let [dest-path (dest-path (assoc params :dest-path-fn identity) (.getPath file) )]
    (ensure-out-dir dest-path true)
    (println (format "Copy file %s to %s" file dest-path))
    (io/copy file (io/file dest-path)) ))


(defn determine-template [{:keys [single-templates] :as params} file]
  (some (fn [[re-string template]] (when (re-find (re-pattern re-string) (.getPath file))
                             template)) single-templates))

(defmulti render (fn [_ file]
                   (file-utils/extension (.getPath file))))

(defmethod render "md" [params file]
  (println (format "Render file %s" file))
  (let [template (determine-template params file)]
    ;(println (format "DEBUG Render file %s %s %s" file template params))
    ;(println (type template))
    (-> file
        slurp
        md/process
        template
        hiccup/html)))

;(re-find #"^blog/.*\.md$" "blog/offer.md")

(defn export-html [params src-file html]
  (let [dest-file (dest-path params src-file)
        dest-dir  (dest-path (assoc params :dest-path-fn drop-extension) src-file)
        dest-index-html-file (str dest-dir "/index.html")]
    (println (format "Export HTML to file %s" dest-file))
    (when (and html src-file (not (.isDirectory src-file)))
      (spit dest-file html))
    (println (format "Export HTML to %s" dest-index-html-file))
    (when (and html src-file)
      (file-utils/create-dirs! dest-dir)
      (spit dest-index-html-file html))))


(def ALL_MARKDOWN_FILES_EXCEPT_DRAFTS #"(?!DRAFT).*\.md")


(defn build-dir! [{:as params :keys [baseurl from aggregate-templates to export-rss?]} src-dir]
  ;;list markdowns file except the ones starting with DRAFT.
  (let [all-markdowns-with-meta (md/list-markdowns-with-meta src-dir ALL_MARKDOWN_FILES_EXCEPT_DRAFTS)]
    (when export-rss?
      (export-rss! params all-markdowns-with-meta))
    (when aggregate-templates
      (doseq [template aggregate-templates]
                                        ;(println "all-markdowns-with-meta" template src-dir all-markdowns-with-meta)
        (let [content   (template all-markdowns-with-meta)
              dest-file (str to (file-separator) "index.html")]
          (println (format "Build directory %s with aggregate-template %s to %s" from (str template) to))
          (spit dest-file (hiccup/html content)))))))

(defn build-file! [{:as params :keys [from to as-assets? compile-opts]} src-file]
  (when (and from (.isDirectory (io/file from)))
    (ensure-out-dir to false))
  (when-let [{:keys [ext]} (and src-file (file-utils/parse-path src-file))]
    (println (format "Build file %s with extension %s" src-file ext))
    (cond
      (or as-assets? (copied-filetypes ext)) (copy-asset! params src-file)
      (rendered-filetypes ext)               (->> (render params src-file)
                                                  (export-html params src-file)))))

(defn build!
  "Builds a static web site based on the content specified in specs. Each build-desc should be a mapping of paths, with additional
  details about how to build data from one path to the other. Available build params keys are:
    * `:from`        - (required) Path from which to build
    * `:to`          - (required) Compiled files go here
    * `:single-templates` - Map of Regex that match the file path to template as Function which takes :hiccup and :metadata and returns some new hiccup, presumably placing the content in question in some place
    * `:aggregate-templates` - vector of templates as function wich takes the return of `all-markdowns-with-meta`

    * `:out-path-fn` - Function used for naming compilation output
    * `:baseurl` - website will be served from this base URL
    * `:to-format`   - Literal format to use for export!
    * `:to-format-fn` - Function of input filename to format
    * `export-rss?` - export RSS feed (default: true)
    * `:as-assets?` - Pass through as a static assets (for images, css, json or edn data, etc)
      - Note: by default, images, css, etc will pass through anyway
  Additional options pertinent to the entire build process may be passed in:
    * `:lazy?` - If true, don't build anything until it changes; this is best for interactive/incremental updates and focused work.
                 Set to false if you want to rebuild from scratch. (default true)
    * `:root-dir` - Static assets will be served relative to this directory (defaults to greatest-common-path between all paths)
  "
  [params]
  (let [{:keys [from] :as params} (merge {:dest-path-fn html-extension} params)
        files (file-seq (io/file from))
        markdowns (map (fn [file] ) files)]
    (build-dir! params from)
    (doseq [src-file files]
      (build-file! params src-file))))

(defn reload-safari-tab! [s]
  (println (str  "Reload Safari tab containing \"" s "\""))
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
     `(def-render-builder {:to          ~export-file
                   :render-fn   "render"
                   :reload-word ~project-name})))
  ([{:keys [to render-fn reload-word] :as params}]
   `(do
      (require 'environ.core)
      (println (format "Def Staticly builder: rendering function \"%s\" exporting HTML to %s" ~render-fn ~to))
      (emit-export ~to)
      (emit-build ~reload-word ~render-fn)
      (emit-main ~render-fn)
      (emit-dev-build)
      nil)))

(defmacro emit-md-build [params]
  `(defn ~(symbol BUILD_FN_NAME) []
     (staticly/build! ~params)
     (when (not= "CLOUDFLARE" (environ.core/env :build-context))
       (reload-safari-tab! (:reload-word ~params)))))

(defn assert-symbol-present?
  "Check if the symbol is present in the ns it is invoked in"
  [s]
  (assert (resolve (symbol s)) (format "Symbol %s must be present in ns %s" s *ns*)))

(defmacro default-blog-params []
  (assert-symbol-present? "post-template")
  (assert-symbol-present? "home-template")
  (assert-symbol-present? "tag-template")
  (let [{:keys [project-name doc-name]} (execution-context)]
    `{:from                ~doc-name
      :to                  (str PUBLIC_DIR ~doc-name)
      :single-templates    {(str "^.*" ~doc-name "/.*\\.md$") ~(symbol "post-template")}
      :aggregate-templates [~(symbol "home-template") ~(symbol "tag-template")]
      :export-rss?         true
      :reload-word         ~project-name}))

(defmacro def-blog-builder
  ([]
   `(def-blog-builder {}))
  ([params]
   `(do
      (let [params# (merge (default-blog-params) ~params)]
        (require 'environ.core)
        (println (format "Define Staticly Blog builder: markdowns in %s dir rendered using single-template %s and aggregate-template %s exported to %s" (:from params#) (:single-templates params#) (:aggregate-templates params#) (:to params#)))
        (emit-md-build params#)
        (when (not= "CLOUDFLARE" (environ.core/env :build-context))
          (~(symbol (str *ns*) BUILD_FN_NAME))
          (staticly/start-watcher! (:from params#) ~(symbol (str *ns*) BUILD_FN_NAME)))))))

(defmacro emit-page-build [params]
  `(defn ~(symbol BUILD_FN_NAME) []
     (staticly/build! ~params)
     (when (not= "CLOUDFLARE" (environ.core/env :build-context))
       (staticly/reload-safari-tab! ~(:reload-word params)))))

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
      (println (format "Define Staticly builder for %s: rendering markdowns in \"%s\" dir using single-templates mapping %s exported to dir \"%s\"" ~(str *ns*) ~from ~single-templates ~to))
      (emit-page-build ~params)
      (when (not= "CLOUDFLARE" (environ.core/env :build-context))
        (~(symbol (str *ns*) BUILD_FN_NAME))
        (staticly/start-watcher! ~from ~(symbol (str *ns*) BUILD_FN_NAME))))))
