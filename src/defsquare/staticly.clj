(ns defsquare.staticly
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]

            [environ.core :as environ]
            [hiccup.core :as hiccup]

            [defsquare.hiccup :as dh]
            [defsquare.markdown :as md]
            [defsquare.file-utils :as file-utils :refer [canonical-path relative-path strip-path-seps join-paths drop-extension html-extension extension file-separator ensure-out-dir]]
            [defsquare.rss :refer [export-rss!]]
            [defsquare.watcher :as watcher :refer [start-watcher!]]
            [defsquare.safari :as safari :refer [reload-safari-tab!]]
            [defsquare.staticly :as staticly])
  (:import [java.io File]))

(def rendered-filetypes #{"md" "mds" "clj" "cljc" "cljs" "yaml" "json" "edn"})

(def copied-filetypes #{"jpg" "png" "svg" "css" "html"})

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


(defn determine-template [{:keys [single-templates] :as params} file]
  (some (fn [[re-string template]] (when (re-find (re-pattern re-string) (.getPath file))
                             template)) single-templates))

(defmulti build-file! (fn [_ file]
                        (let [ext (file-utils/extension (.getPath file))]
                          (println "build-file! %s with extension %s" file ext)
                          (cond
                            (copied-filetypes ext)   :copied
                            (rendered-filetypes ext) :rendered))))

(defn write-html [params src-file html]
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

(defmethod build-file! :rendered [{:keys [from to] :as params} file]
  (println (format "Build/render file %s" file))
  (let [template       (determine-template params file)
        markdown       (-> file
                           slurp
                           md/process)
        templated-html (-> markdown
                           template
                           hiccup/html)]
    ;(println (format "DEBUG Render file %s %s %s" file template params))
    ;(println (type template))
    (when (and from (.isDirectory (io/file from)))
      (ensure-out-dir to false))
    (write-html params file templated-html)
    (assoc markdown :templated-html templated-html :type :markdown :file file)))

(defn copy-asset! [params file]
  (let [dest-path (dest-path (assoc params :dest-path-fn identity) (.getPath file) )]
    (ensure-out-dir dest-path true)
    (println (format "Copy file %s to %s" file dest-path))
    (io/copy file (io/file dest-path))
    {:dest-path dest-path :file file :type :asset}))

(defmethod build-file! :copied [params file]
  (println (format "Build/copy file %s" file))
  (copy-asset! params file))

;(re-find #"^blog/.*\.md$" "blog/offer.md")

(def ALL_MARKDOWN_FILES_EXCEPT_DRAFTS #"(?!DRAFT).*\.md")

(defn build-dir! [{:as params :keys [baseurl from aggregate-templates to export-rss?]} src-dir markdowns]
  ;;list markdowns file except the ones starting with DRAFT.
  (let [all-markdowns-with-meta (md/list-markdowns-with-meta src-dir ALL_MARKDOWN_FILES_EXCEPT_DRAFTS)]
    (when export-rss?
      (export-rss! params all-markdowns-with-meta))
    (when aggregate-templates
      (doseq [{:keys [template-fn file-name]} aggregate-templates]
        (let [content   (template-fn all-markdowns-with-meta)
              dest-file (str to (file-separator) file-name)]
          (println "all-markdowns-with-meta" dest-file template-fn src-dir all-markdowns-with-meta)
          (println (format "Build directory %s with aggregate-template %s to %s" from (str template-fn) to))
          (spit dest-file (hiccup/html content)))))))

(defn markdown-extension? [file] (= "md" (:ext (file-utils/parse-path file))))

(defn extract-and-render [params files]
  (map (fn [file]
         (when (markdown-extension? file)
           (build-file! params file))
         files)))

(defn- filter-out-draft-markdown [built-files]
  (filter (fn [built-file]
            (and (= :markdown (:type built-file))
                 built-file
                 (re-matches ALL_MARKDOWN_FILES_EXCEPT_DRAFTS (:base (file-utils/parse-path (:file built-file)))))) built-files))

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
  Additional options pertinent to the entire build process may be passed in:
    * `:lazy?` - If true, don't build anything until it changes; this is best for interactive/incremental updates and focused work.
                 Set to false if you want to rebuild from scratch. (default true)
    * `:root-dir` - Static assets will be served relative to this directory (defaults to greatest-common-path between all paths)
  "
  [params]
  (let [{:keys [from export-rss?] :as params} (merge {:dest-path-fn html-extension} params)
        files (file-utils/list-files from)
        built-files (map (partial build-file! params) files)
        markdowns (filter-out-draft-markdown built-files)]
    (when export-rss?
      (export-rss! params markdowns))
    (build-dir! params from markdowns)))

(defn ns-last-name [ns]
  (subs (str ns) (inc (clojure.string/last-index-of (str ns) "."))))

(defn ns-first-name [ns]
  (subs (str ns) 0 (.indexOf (str ns) ".")))

(defn execution-context []
  {:project-name (ns-first-name *ns*)
   :doc-name     (ns-last-name *ns*)})

(defn reload-word []
  (:project-name (execution-context)))

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

(def build-fns (atom []))
(defn register-build-function! [build-fn-var]
  (swap! build-fns conj build-fn-var))

(def CI_ENVIRONMENT_BUILD_CONTEXT "CLOUDFLARE")

(defn developer-environment? []
  (not= CI_ENVIRONMENT_BUILD_CONTEXT (environ.core/env :build-context)))

(defn reload-browser! []
  (when (developer-environment?)
    (safari/reload-safari-tab! (reload-word))))

(defmacro emit-build [render-fn]
  `(do (defn ~(symbol BUILD_FN_NAME) []
         ;;invoke export
         (~(symbol EXPORT_FN_NAME) (~(symbol (str *ns*) render-fn)))
         (staticly/reload-browser!))
       (staticly/register-build-function! (var ~(symbol BUILD_FN_NAME)))))

(defmacro emit-main [render-fn]
  `(defn ~(symbol "-main") [& args#]
     (~(symbol EXPORT_FN_NAME) (~ (symbol (str *ns*) render-fn)))
     (shutdown-agents)))

(defmacro emit-dev-build []
  `(when (staticly/developer-environment?)
     (~(symbol (str *ns*) BUILD_FN_NAME))))


(defmacro def-render-builder
  ([]
   (let [{:keys [project-name doc-name]} (execution-context)
         export-dir                      PUBLIC_DIR
         export-file                     (str export-dir doc-name ".html")]
     `(def-render-builder {:to          ~export-file
                           :render-fn   "render"})))
  ([{:keys [to render-fn] :as params}]
   `(do
      (require 'environ.core)
      (println (format "Def Staticly builder: rendering function \"%s\" exporting HTML to %s" ~render-fn ~to))
      (emit-export ~to)
      (emit-build ~render-fn)
      (emit-main ~render-fn)
      (emit-dev-build)
      nil)))

(defmacro emit-md-build [params]
  `(do (defn ~(symbol BUILD_FN_NAME) []
         (staticly/build! ~params)
         (staticly/reload-browser!))
       (staticly/register-build-function! (var ~(symbol BUILD_FN_NAME)))))

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
      :aggregate-templates [{:template-fn ~(symbol "home-template") :file-name "index.html"} {:template-fn ~(symbol "tag-template") :file-name "tags.html"}]
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
        (when (staticly/developer-environment?)
          (~(symbol (str *ns*) BUILD_FN_NAME))
          (watcher/start-watcher! ~(defsquare.file-utils/parent *file*) ~(symbol (str *ns*) BUILD_FN_NAME)))))))

(defmacro emit-page-build [params]
  `(do (defn ~(symbol BUILD_FN_NAME) []
         (staticly/build! ~params)
         (staticly/reload-browser!))
       (staticly/register-build-function! (var ~(symbol BUILD_FN_NAME)))))

(defmacro def-page-builder
  ([]
   (let [{:keys [doc-name]} (execution-context)]
     `(def-page-builder {:from                ~doc-name
                         :to                  ~PUBLIC_DIR
                         :single-templates    {(str "^.*" ~doc-name "/.*\\.md$") ~(symbol "page-template")}})))
  ([{:keys [from to single-templates] :as params}]
   `(do
      (require 'environ.core)
      (println (format "Define Staticly builder for %s: rendering markdowns in \"%s\" dir using single-templates mapping %s exported to dir \"%s\"" ~(str *ns*) ~from ~single-templates ~to))
      (emit-page-build ~params)
      (when (staticly/developer-environment?)
        (~(symbol (str *ns*) BUILD_FN_NAME))
        (watcher/start-watcher! ~(defsquare.file-utils/parent *file*) ~(symbol (str *ns*) BUILD_FN_NAME))))))

(defn watch-build-and-reload! []
  (letfn [(rebuild-and-reload! []
            (println "Rebuild and Reload!")
            (doseq [build!-fn @build-fns]
              (build!-fn))
            (reload-browser!))]
    (when (developer-environment?)
      (watcher/start-watcher! (defsquare.file-utils/parent *file*) rebuild-and-reload!)
      (rebuild-and-reload!))))
