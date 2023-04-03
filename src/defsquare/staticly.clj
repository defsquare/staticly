(ns defsquare.staticly
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]

            [environ.core :as environ]
            [hiccup.core :as hiccup]
            [lambdaisland.uri.normalize :as normalize]

            [defsquare.markdown :as md]
            [defsquare.file-utils :as file-utils :refer [exists? canonical-path relative-path as-path directory? file? strip-path-seps join-paths drop-extension html-extension extension file-separator ensure-out-dir]]
            [defsquare.rss :refer [export-rss!]]
            [defsquare.watcher :as watcher :refer [start-watcher!]]
            [defsquare.safari :as safari :refer [reload-safari-tab!]]
            [defsquare.staticly :as staticly])
  (:import [java.io File]))

(def rendered-filetypes #{"md" "mds" "clj" "cljc" "cljs" "yaml" "json" "edn"})

(def copied-filetypes #{"jpg" "png" "svg" "css" "html" "js"})


(defn lower
  "Converts string to all lower-case.
  This function works in strictly locale independent way,
  if you want a localized version, just use `locale-lower`"
  [s]
  (when (string? s)
    (.toLowerCase ^String s)))

(def ^:private +slug-tr-map+
  (zipmap "ąàáäâãåæăćčĉęèéëêĝĥìíïîĵłľńňòóöőôõðøśșšŝťțŭùúüűûñÿýçżźž"
          "aaaaaaaaaccceeeeeghiiiijllnnoooooooossssttuuuuuunyyczzz"))

(defn replace
  "Replaces all instance of match with replacement in s.
  The replacement is literal (i.e. none of its characters are treated
  specially) for all cases above except pattern / string.
  In match is pattern instance, replacement can contain $1, $2, etc.
  will be substituted with string that matcher the corresponding
  parenthesized group in pattern.
  If you wish your replacement string to be used literary,
  use `(cuerdas.regexp/escape replacement)`.
  Example:
    (replace \"Almost Pig Latin\" #\"\\b(\\w)(\\w+)\\b\" \"$2$1ay\")
    ;; => \"lmostAay igPay atinLay\"
  "
  [s match replacement]
  (when (string? s)
    (str/replace s match replacement)))

(defn slug "Transform text into a URL slug." [s]
  (some-> (lower s)
          (str/escape +slug-tr-map+)
          (replace #"[^\w\s]+" "")
          (replace #"\s+" "-")))

(defn assert-symbol-present?
  "Check if the symbol as this string is present in the ns it is invoked in, returns the symbol"
  [s]
  (let [sym (resolve (symbol s))]
    (assert sym (format "Symbol %s must be present in ns %s" s *ns*))
    sym))

(defn assert-fn-present?
  "Check if the symbol as this string is present in the ns it is invoked in and it is bind to a function"
  [s]
  (let [sym (assert-symbol-present? s)]
    (assert (fn? (var-get sym)) (format "Symbol %s in ns %s must be binded to a function" s *ns*))
    sym))

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
                          (log/infof "build-file! %s with extension %s" (str file) ext)
                          (cond
                            (copied-filetypes ext)   :copy
                            (rendered-filetypes ext) :render))))

(defn write-html [params src-file html]
  (let [dest-file (dest-path params src-file)
        dest-dir  (dest-path (assoc params :dest-path-fn drop-extension) src-file)
        dest-index-html-file (str dest-dir "/index.html")]
    (log/infof "Write HTML to file %s" dest-file)
    (when (and html src-file (not (.isDirectory src-file)))
      (spit dest-file html))
    (log/infof "Write HTML to %s" dest-index-html-file)
    (when (and html src-file)
      (file-utils/create-dirs! dest-dir)
      (spit dest-index-html-file html))))

(defmethod build-file! :render [{:keys [from to single-templates] :as params} file]
  (log/infof "Build/render file %s" file)
  (let [template       (get single-templates 0)
        markdown       (md/process-file file)
      ;  _ (println "markdown " template)
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
    (log/infof "Copy file %s to %s" file dest-path)
    (io/copy file (io/file dest-path))
    {:dest-path dest-path :file file :type :asset}))

(defmethod build-file! :copy [params file]
  (log/infof "Build/copy file %s" file)
  (copy-asset! params file))

;(re-find #"^blog/.*\.md$" "blog/offer.md")

(def ALL_MARKDOWN_FILES_EXCEPT_DRAFTS #"(?!DRAFT).*\.md")

(defn build-dir! [{:as params :keys [baseurl from aggregate-templates to export-rss?]} src-dir markdowns]
  ;;list markdowns file except the ones starting with DRAFT.
  (let [all-markdowns-with-meta (md/list-markdowns-with-meta src-dir ALL_MARKDOWN_FILES_EXCEPT_DRAFTS)]
    ;(when export-rss? (export-rss! params markdowns))
    (when aggregate-templates
      (doseq [{:keys [template-fn file-name] :as template} aggregate-templates]
        (let [content   (template-fn all-markdowns-with-meta)
              dest-file (str to (file-separator) file-name)]
          ;(log/infof "all-markdowns-with-meta" dest-file template-fn src-dir all-markdowns-with-meta)
          (log/infof "Build directory %s with aggregate-template %s to %s" from (str template-fn) to)
          (spit dest-file (hiccup/html content)))))))

(defn markdown-extension? [file] (= "md" (:ext (file-utils/parse-path file))))

(defn- filter-out-draft-markdown [built-files]
  (filter (fn [built-file]
            (and (= :markdown (:type built-file))
                 built-file
                 (re-matches ALL_MARKDOWN_FILES_EXCEPT_DRAFTS (:base (file-utils/parse-path (:file built-file)))))) built-files))

(defn ns-last-name
  ([] (ns-last-name *ns*))
  ([ns]
   (let [ns-str       (str ns)
         index-of-dot (.indexOf ns-str ".")]
     (if (> index-of-dot -1)
       (subs ns-str (inc (clojure.string/last-index-of ns-str ".")))
       ns-str))))

(defn ns-first-name
  ([] (ns-first-name *ns*))
  ([ns]
   (let [ns-str       (str ns)
         index-of-dot (.indexOf ns-str ".")]
     (if (> index-of-dot -1)
       (subs ns-str 0 index-of-dot)
       ns-str))))

(defn build!
  "Builds a static web site based on the content specified in specs. Each build-desc should be a mapping of paths, with additional
  details about how to build data from one path to the other. Available build params keys are:
    * `:from`        - (required) Seq of paths from which to build, these dirs contains either assets to be copied or md files to be rendered
    * `:to`          - (required) single dir, built files go here (either rendered with a template or copied for assets)
    * `:templates`
       * `:1-1`
       * `:1-n`
       * `:n-1`
       * `:n-n`
    * `:out-path-fn` - Function used for naming compilation output
    * `:baseurl` - website is served from this base URL
    * `:export-rss?` - export RSS feed (default: true) "
  [params]
  (let [{:keys [from export-rss?] :as params} (merge {:dest-path-fn html-extension} params)
        files (file-utils/list-files from)
        built-files (doall (map (partial build-file! params) files))
        markdowns (filter-out-draft-markdown built-files)]
    (when export-rss?
      (export-rss! params markdowns))
    (build-dir! params from markdowns)))

(defn execution-context []
  (let [ns-last-name  (ns-last-name *ns*)
        ns-first-name (ns-first-name *ns*)]
    {:ns-last-name  ns-last-name
     :ns-first-name ns-first-name
     :doc-name      ns-last-name
     :project-name  ns-first-name}))

(defn reload-word []
  (:project-name (execution-context)))

(def BUILD_FN_NAME "build!")
(def EXPORT_FN_NAME "export!")
(def PUBLIC_DIR "resources/public")

(defn export! [hiccup filename]
  (log/infof "Export html to %s" filename)
  (let [to (io/as-file filename)]
    (when (file-utils/directory? filename) (file-utils/ensure-out-dir filename))
    (when (file-utils/file? filename)      (file-utils/ensure-out-dir (str (file-utils/as-path (file-utils/parent filename)))))
    (spit to (hiccup/html hiccup))))

(defmacro emit-export! [filename]
  `(defn ~(symbol EXPORT_FN_NAME)
     ([hiccup#]
      (~(symbol EXPORT_FN_NAME) hiccup# ~filename))
     ([hiccup# filename#]
      (export! hiccup# filename#))))

(def build-fns (atom []))
(defn register-build-function! [build-fn-var]
  (swap! build-fns conj build-fn-var))

(def CI_ENVIRONMENT_BUILD_CONTEXT "CLOUDFLARE")

(defn developer-environment? []
  (not= CI_ENVIRONMENT_BUILD_CONTEXT (environ.core/env :build-context)))

(defn reload-browser! []
  (when (developer-environment?)
    (safari/reload-safari-tab! (reload-word))))


(defmacro emit-main [params]
  `(defn ~(symbol "-main") [& args#]
     (build-render! ~params)
     (shutdown-agents)))

(defmacro build!-in-dev-env []
  `(when (staticly/developer-environment?)
     (~(symbol (str *ns*) BUILD_FN_NAME))))

(defmacro current-file []
  `(-> (or (clojure.java.io/resource *file*) (clojure.java.io/file *file*))
       .toURI
       (java.nio.file.Paths/get)
       .toString))

(defn write-html!
  ([to docname-to-hiccup]
   (mapcat (fn [[docname hiccup]] (write-html! to docname hiccup)) docname-to-hiccup))
  ([to docname hiccup]
   (let [dest-file            (io/as-file (str to (file-separator) docname ".html"))
         dest-dir             (str to (file-separator) docname)
         dest-index-html-file (io/as-file (str dest-dir (file-separator) "index.html"))
         html                 (when hiccup (hiccup/html hiccup))]
     (log/infof "Write HTML to file %s" dest-file)
     (when (and html dest-file)
       (file-utils/ensure-out-dir (str to))
       (spit dest-file html))
     (log/infof "Write HTML as index.html in directory %s" dest-dir )
     (when (and html dest-dir)
                                        ;(file-utils/create-dirs! dest-dir)
       (file-utils/ensure-out-dir dest-dir)
       ;(println "spit" dest-dir dest-index-html-file (exists? dest-dir))
       (spit dest-index-html-file html))
     [dest-file dest-index-html-file])))

(defn add-docname-if-missing [x]
  (if (vector? x)
    ;;the docname is the namespace last name
    {(ns-last-name *ns*) x}
    (if (map? x)
      x
      (throw (ex-info "Render function must return either hiccup or a map of docname to hiccup")))))

(defn build-render!
  "When invoked in a ns, it invokes the 'render' function that must return either hicupp or a map of docname to hiccup, then converts hiccup to html and write it"
  [& {:keys [to render-fn] :or {to PUBLIC_DIR render-fn "render"}}]
  (assert-fn-present? "render")
  (let [render-fn              (resolve (symbol (str *ns*) render-fn))
        [docname hiccup]       (first (add-docname-if-missing (render-fn)))]
    (println *ns* to render-fn docname)
    (write-html! to docname hiccup)))

(defmacro emit-build! [params]
  `(do (defn ~(symbol BUILD_FN_NAME) []
         (let [files# (build-render! ~params)]
           (staticly/reload-browser!)
           files#))
       (staticly/register-build-function! (var ~(symbol BUILD_FN_NAME)))))

(defmacro def-render-builder
  ([] `(def-render-builder {:to ~PUBLIC_DIR :render-fn "render"}))
  ([{:keys [to render-fn] :or {to PUBLIC_DIR render-fn "render"} :as params}]
   (log/infof "Def Staticly builder: rendering function \"%s\" writing HTML to %s" render-fn to)
   `(do
      (require 'environ.core)
      (emit-build! ~(assoc params :to to :render-fn render-fn))
      (emit-main  ~(assoc params :to to :render-fn render-fn))
      (def ~(symbol "html-files") (build!-in-dev-env))
      ;watch the clj file
      (watcher/start-watcher! ~(current-file) ~(symbol (str *ns*) BUILD_FN_NAME))
      nil)))

(defmacro emit-md-build [params]
  `(do (defn ~(symbol BUILD_FN_NAME) []
         (staticly/build! ~params)
         (staticly/reload-browser!))
       (staticly/register-build-function! (var ~(symbol BUILD_FN_NAME)))))

(defn index-by-values
  "return a map with keys as the values found from applying f to element of coll (must return a coll) and the values a seq of all the element with that value"
  [coll f]
  (reduce (fn [aggreg e]
            (reduce (fn [aggreg v]
                      (if (contains? aggreg v)
                        (assoc aggreg v (conj (get aggreg v) e))
                        (assoc aggreg v [e]))) aggreg (f e))) {} coll))

(defn index-by
  "return a map with keys as the value found from applying f to element of coll (must return a value acceptable as key) and the value a seq of all the element with that value"
  [coll f]
  (reduce (fn [aggreg e]
            (let [v (f e)]
              (if (contains? aggreg v)
                (assoc aggreg v (conj (get aggreg v) e))
                (assoc aggreg v [e])))) {} coll))

(comment
  (index-by-values [{:k1 1 :tags [:a :b]} {:k1 2 :tags [:a :b :c]}] :tags)
  (index-by [{:k 1 :author "j"} {:k 2 :author "j"} {:k 3 :author "d"}] :author))

(defn aggregate-by-tags
  "extract each tags of every markdown metadata and return a map with tag as key and value a vector of markdown (metadata + raw + hiccup)"
  [markdowns]
  (->> (index-by-values markdowns #(get-in % [:metadata :tags]))
       (reduce-kv (fn [m k markdowns]
                    ;normalize key as they will be used in URI
                    (assoc m (slug k) markdowns)) {})))

(defn aggregate-by-authors
  "extract each author of every markdown metadata and return a map with author as key and value a vector of markdown (metadata + raw + hiccup)"
  [markdowns]
  (->> (index-by-values markdowns #(get-in % [:metadata :authors]))
       (reduce-kv (fn [m k markdowns]
                    ;normalize key as they will be used in URI
                    (assoc m (slug k) markdowns)) {} )))

(defmacro default-blog-params []
  (assert-symbol-present? "post-template")
  (assert-symbol-present? "home-template")
  (assert-symbol-present? "tag-template")
  (let [{:keys [project-name doc-name]} (execution-context)]
    `{:from                ~doc-name
      :to                  (str PUBLIC_DIR ~doc-name)
      :templates {:1-1 [~(symbol "post-template")]
                  :n-1 [{:template-fn ~(symbol "home-template")}
                        {:template-fn ~(symbol "tag-template")  :file-name "tags.html"}]}
      :single-templates    [~(symbol "post-template")]
      :aggregate-templates [{:template-fn ~(symbol "home-template") :file-name "index.html"}
                            {:template-fn ~(symbol "tag-template")  :file-name "tags.html"}]
      :export-rss?         true
      :reload-word         ~project-name}))

(defn default-blog-params []
  (let [{:keys [project-name doc-name]} (execution-context)]
    {:from        "blog"
     :to          (str PUBLIC_DIR "/" doc-name)
     :templates   {:1-1 [
                        {:template-fn-name "post-template" :excludes nil :includes [#"\\*.md$"] }
                        ]
                   :n-1 [
                         {:template-fn-name "home-template"   :includes [#"\\*.md$"] :excludes [#"DRAFT\\*md"]              :name "home"}
                         {:template-fn-name "tag-template"    :includes [#"\\*.md$"] :excludes [#"DRAFT\\*md"] :aggregation-fn aggregate-by-tags    :prefix "tags"    :name "tags"}
                         {:template-fn-name "author-template" :includes [#"\\*.md$"] :excludes [#"DRAFT\\*md"] :aggregation-fn aggregate-by-authors :prefix "authors" :name "authors"}
                         ]}
     :export-rss? true
     :reload-word project-name}))

(defn resolve-fn [fn-name]
  (resolve (symbol (str *ns*) fn-name)))

(defn invoke-template-1-1 [template-fn files to]
  (into {} (map (fn [file] (let [html-files (->> file
                                                md/process-file
                                                template-fn
                                                add-docname-if-missing
                                                (write-html! to))]
                            [file html-files])) files)))

(defn invoke-template-n-1 [template-fn files to]
  (let [[html-file index-html-file-in-dir] (->> files
                                                (map md/process-file)
                                                template-fn
                                                add-docname-if-missing
                                                (write-html! to))]
    {html-file              files
     index-html-file-in-dir files}))

(defn invoke-aggregation-template [template-fn aggregation-value->files to]
  (println "invoke aggregation template" template-fn)
  (into {} (map (fn [[aggregation-value files]]
                  (let [html-files (->> (template-fn aggregation-value files)
                                        add-docname-if-missing
                                        (write-html! to))]
                    [aggregation-value html-files])) aggregation-value->files)))

(defn build-blog!
  "Build a blog like structure with parameters as (see default-blog-params)"
  [& {:keys [from to templates export-rss? reload-word]}]
  (let [default       (default-blog-params)
        from          (or from  (:from default))
        to            (or to  (:to default))
        templates     (or templates (:templates default))
        export-rss?   (or export-rss? (:export-rss? default))
        reload-word   (or reload-word  (:reload-word default))
        templates-1-1 (get templates :1-1)
        templates-n-1 (get templates :n-1)]
    (log/infof "Build blog from %s to %s with templates %s" from to templates)
    {:1-1 (reduce (fn [acc {:keys [template-fn-name includes excludes] :as template-1-1}]
                    (assert-fn-present? template-fn-name)
                    (let [template-fn      (resolve-fn template-fn-name)
                          files            (file-utils/list-files from includes excludes)
                          input-to-outputs (invoke-template-1-1 template-fn files to)]
                      (merge acc input-to-outputs)) )
                  {} templates-1-1)
     :n-1 (reduce (fn [acc {:keys [template-fn-name includes excludes aggregation-fn prefix name] :as template-n-1}]
                    (assert-fn-present? template-fn-name)
                    (let [template-fn  (resolve-fn template-fn-name)
                          outputs      (if aggregation-fn
                                         (invoke-aggregation-template template-fn (aggregation-fn (md/process-files from includes excludes)) (if prefix (str to (file-separator) prefix) to))
                                         (invoke-template-n-1         template-fn (file-utils/list-files from includes excludes)             (if prefix (str to (file-separator) prefix) to)))]
                      (assoc acc name outputs)))
                  {} templates-n-1)}))

(defmacro emit-blog-build [params]
  `(do (defn ~(symbol BUILD_FN_NAME) []
         (let [outputs# (staticly/build-blog! ~params)]
           (when (staticly/developer-environment?)
             (staticly/reload-browser!))
           outputs#))
       (staticly/register-build-function! (var ~(symbol BUILD_FN_NAME)))))

(defmacro def-blog-builder
  ([]
   `(def-blog-builder {}))
  ([{:keys [from to templates export-rss? reload-word]}]
   (println "def-blog-builder" from to templates)
   (let [default     (default-blog-params)
         from        (or from (:from default))
         to          (or (when to (str (var-get (resolve to)))) (:to default))
         templates   (or templates (:templates default))
         export-rss? (or export-rss? (:export-rss? default))
         reload-word (or reload-word  (:reload-word default))
         params      {:from from :to to :templates templates}]
     (assert (exists? from) (format "The directory '%s' must exists for the page builder to find markdown files in it" from))
     `(do
        (let [params# (merge (default-blog-params) ~params)]

          (require 'environ.core)
          (log/infof "Define Staticly Blog builder: markdowns in %s dir rendered using 1-1 %s and n-1 %s exported to %s" (:from params#) (:1-1 (:templates params#)) (:n-1 (:templates params#)) (:to params#))
          (emit-blog-build params#)
          (def ~(symbol "export-dir") ~to)
          (when (staticly/developer-environment?)
            ;;execute the function and bind the result to a var
            (def ~(symbol "outputs") (~(symbol (str *ns*) BUILD_FN_NAME)))
                                        ;watch the clj file
            (watcher/start-watcher! ~(current-file) ~(symbol (str *ns*) BUILD_FN_NAME))
            ;;watch the 'blog' folder md files
                                        ;(println (:from params#))
            (watcher/start-watcher! ~from ~(symbol (str *ns*) BUILD_FN_NAME))
            ~(symbol "outputs")))))))

(def DEFAULT_PAGE_PARAMS {:from "pages"
                          :to PUBLIC_DIR
                          :templates {:1-1 [ {:includes [#"\\*.md$"] :template-fn-name "page-template" :excludes nil}]}})


(defn build-page! [& {:keys [from to templates] :or {from      (:from DEFAULT_PAGE_PARAMS)
                                                     to        (:to DEFAULT_PAGE_PARAMS)
                                                     templates (:templates DEFAULT_PAGE_PARAMS) } :as params}]
  ;(println "build-page!" params from to templates)
  (when (not templates) (log/info "No templates defined in params, doing nothing"))
  (reduce (fn [acc {:keys [template-fn-name includes excludes] :as template-1-1}]
            (assert-fn-present? template-fn-name)
            (let [files            (file-utils/list-files from includes excludes)
                  template-fn      (resolve (symbol (str *ns*) template-fn-name))
                  input-to-outputs (invoke-template-1-1 template-fn files to)]
              (merge acc input-to-outputs)) ) {} (:1-1 templates)))

(defmacro emit-page-build [params]
  `(do (defn ~(symbol BUILD_FN_NAME) []
         (let [written-files# (staticly/build-page! ~params)]
           (when (staticly/developer-environment?)
             (staticly/reload-browser!))
           written-files#))
       (staticly/register-build-function! (var ~(symbol BUILD_FN_NAME)))))

(defmacro def-page-builder
  ([]
   (let [{:keys [doc-name]} (execution-context)]
     `(def-page-builder {:from                ~doc-name
                         :to                  ~PUBLIC_DIR})))
  ([{:keys [from to templates]}]
   (let [{:keys [doc-name]} (execution-context)
         from (or from doc-name)
         to (str (var-get (resolve to)))
         templates (or templates (:templates DEFAULT_PAGE_PARAMS))
         params {:from from :to to :templates templates}]
     (assert (exists? from) (format "The directory '%s' must exists for the page builder to find markdown files in it" from))
     ;(println "def-page-builder" from to single-templates doc-name)
     `(do
        (require 'environ.core)
        (log/infof "Define Staticly builder for %s: rendering markdowns in \"%s\" dir using 1-1 templates mapping %s exported to dir \"%s\"" ~(str *ns*) ~from ~templates ~to)
        (emit-page-build ~params)
        (def ~(symbol "export-dir") ~to)
        (when (staticly/developer-environment?)
          ;;execute the function and bind the result to a var
          (def ~(symbol "input-to-outputs") (~(symbol (str *ns*) BUILD_FN_NAME)))
          ;;watch the clj file
          (watcher/start-watcher! ~(current-file) ~(symbol (str *ns*) BUILD_FN_NAME))
          ;;watch the folder where sits the markdown files
          (watcher/start-watcher! ~from ~(symbol (str *ns*) BUILD_FN_NAME))
          ~(symbol "input-to-outputs"))))))

(defn rebuild-and-reload! []
  (log/info "Rebuild and Reload!")
  (doseq [build!-fn @build-fns]
    (build!-fn))
  (reload-browser!))

(defmacro watch-build-and-reload! []
  (when (developer-environment?)
    `(do
      ;;watch the file where this macro is invoked
      (watcher/start-watcher! ~(current-file) rebuild-and-reload!)
      (rebuild-and-reload!))))


(comment
  (let [files       (file-utils/list-files "blog")
        files       (md/list-markdowns-with-meta "blog")
        built-files (doall (map (partial build-file! params) files))])
  ;(index-by-values (md/list-markdowns-with-meta "blog") :tags)
  {:from                ~doc-name
   :to                  (str PUBLIC_DIR ~doc-name)
   :single-templates    [~(symbol "post-template")]
   :aggregate-templates [{:template-fn ~(symbol "home-template") :file-name "index.html"}
                         {:template-fn ~(symbol "tag-template") :file-name "tags.html"}]
   :export-rss?         true
   :reload-word         ~project-name})
