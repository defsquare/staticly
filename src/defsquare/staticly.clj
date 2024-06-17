(ns defsquare.staticly
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]

            [environ.core :as environ]
            [hiccup.core :as hiccup]
            [lambdaisland.uri.normalize :as normalize]
;            [mount.lite :refer [defstate]]
            [defsquare.markdown :as md]
            [defsquare.files :as file-utils :refer [exists? canonical-path relative-path as-path directory? file? strip-path-seps join-paths drop-extension html-extension extension file-separator ensure-out-dir]]
            [defsquare.rss :refer [write-rss!]]
            [defsquare.watcher :as watcher :refer [start-watcher!]]
            [defsquare.safari :as safari :refer [reload-tab!]]
            [defsquare.server :as server]
            )
  (:import [java.io File])
  (:gen-class))

(set! *warn-on-reflection* true)

(def rendered-filetypes #{"md" "mds" "clj" "cljc" "cljs" "yaml" "json" "edn"})

(def copied-filetypes #{"jpg" "jpeg"  "png" "svg" "css" "html" "js" "ttf" "woff" "woff2" "eot" "ico"})

;(defstate http-server :start (server/start-server! ))

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

(defn string-replace
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
          (string-replace #"[^\w\s]+" "")
          (string-replace #"\s+" "-")))

(defn assert-symbol-present?
  "Check if the symbol as this string is present in the ns it is invoked in, returns the symbol"
  ([s]
   (let [sym (resolve (symbol s))]
     (assert sym (format "Symbol %s must be present in ns %s" s *ns*))
     sym))
  ([ns s]
   (let [sym (resolve (symbol (str ns) s))]
     (assert sym (format "Symbol %s must be present in ns %s" s (str ns)))
     sym)))

(defn assert-fn-present?
  "Check if the symbol as this string is present in the ns it is invoked in and it is bind to a function"
  ([s]
   (let [sym (assert-symbol-present? s)]
     (assert (fn? (var-get sym)) (format "Symbol %s in ns %s must be binded to a function" s *ns*))
     sym))
  ([ns s]
   (let [sym (assert-symbol-present? ns s)]
     (assert (fn? (var-get sym)) (format "Symbol %s in ns %s must be binded to a function" s ns))
     sym)))

(defn- dest-path
  "return the destination path for this origin file path"
  [from to dest-path-fn path]
  (let [out-path-fn (or dest-path-fn drop-extension)
        single-file? (= path from)
        to-dir? (or (.isDirectory ^File (file-utils/as-file to))
                    (= ^char (last (.getPath ^File (file-utils/as-file path))) ^char (java.io.File/separatorChar)))
        relative-from-path (if single-file? path (relative-path path from))]
    (if (and single-file? (not to-dir?))
      ;; then we're just translating a single file with an explicit `to` path
      to
      ;; then we assume that we're exporting to a path which has a directory created for it
      (join-paths (or to ".")  (out-path-fn relative-from-path)))))

(defn dest-url [{:as params :keys [from baseurl dest-path-fn]} path]
  (str baseurl "/" (dest-path-fn (if (= path from) path (relative-path path from)))))

(defn determine-template [{:keys [single-templates] :as params} ^File file]
  (some (fn [[re-string template]] (when (re-find (re-pattern re-string) (.getPath file))
                             template)) single-templates))

(defmulti build-file! (fn [_ ^File file]
                        (let [ext (file-utils/extension (.getPath file))]
                          (log/infof "build-file! %s with extension %s" (str file) ext)
                          (cond
                            (copied-filetypes ext)   :copy
                            (rendered-filetypes ext) :render))))

(defn write-html [{:keys [from to dest-path-fn]} ^File src-file html]
  (let [dest-file (dest-path from to dest-path-fn src-file)
        dest-dir  (dest-path from to drop-extension src-file)
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

(defn copy-asset! [{:keys [from to]} ^File file]
  (let [dest-path (dest-path from to identity (.getPath file) )]
    (ensure-out-dir dest-path true)
    (log/infof "Copy file %s to %s" file dest-path)
    (io/copy file (io/file dest-path))
    {:dest-path dest-path :file file :type :asset}))

(defn copy-assets! [{:keys [from to dest-path-fn]}]
  (log/infof "Copy assets from %s to %s" from to)
  (reduce (fn [acc dir]
            (let [files (doall (file-utils/list-files dir (fn [^File file] (copied-filetypes (file-utils/extension (.getPath file))) )))]
              (log/infof "Copy asset files: %s" files)
              (reduce (fn [acc ^File file]
                        (let [dest-path (dest-path dir to identity (.getPath file))]
                          (ensure-out-dir dest-path true)
                          (log/debugf "Copy asset file %s to %s" file dest-path)
                          (io/copy file (io/file dest-path))
                          (conj acc {:dest-path dest-path :file file :type :asset})))
                      acc
                      files))) [] from))

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
      (write-rss! params markdowns))
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
(def WRITE_DIR "dist")

(defn export! [hiccup filename]
  (log/infof "Export html to %s" filename)
  (let [to (io/as-file filename)]
    (when (file-utils/directory? filename) (file-utils/ensure-out-dir filename))
    (when (file-utils/file? filename)      (file-utils/ensure-out-dir (str (file-utils/as-path (file-utils/parent filename)))))
    (spit to (hiccup/html hiccup))))

(def build-fns (atom []))
(defn register-build-function! [build-fn-var]
  (swap! build-fns conj build-fn-var))

(def CI_ENVIRONMENT_BUILD_CONTEXT "CLOUDFLARE")

(defn developer-environment? []
  (not= CI_ENVIRONMENT_BUILD_CONTEXT (environ.core/env :build-context)))

(defn reload-browser! []
  (when (developer-environment?)
    (safari/reload-tab! (reload-word))))

(defmacro emit-main [params]
  `(defn ~(symbol "-main") [& args#]
     (build-render! ~params)
     (shutdown-agents)))

(defmacro build!-in-dev-env []
  `(when (staticly/developer-environment?)
     (~(symbol (str *ns*) BUILD_FN_NAME))))

(defmacro current-file []
  `(-> (or ~(vary-meta `(clojure.java.io/resource *file*) assoc :tag `File)
           ~(vary-meta `(clojure.java.io/file *file*) assoc :tag `File))
       ^File
       .toURI
       (java.nio.file.Paths/get)
       .toString))

(defn content-type [x]
  (cond (string? x) :string
        (vector? x) :hiccup))

(defn render-content-if-needed [x]
  (when x
    (case (content-type x)
      :string x;write the content as is
      :hiccup (hiccup/html x)
      x)))

(defn write-content!
  ([to extension docname-to-content]
   ;(println "write-content!" to extension docname-to-content)
   (mapcat (fn [[docname content]] (write-content! to extension docname content)) docname-to-content))
  ([to extension docname content]
   (let [dest-file            (io/as-file (str to (file-separator) docname "." extension))
         dest-dir             (str to (file-separator) docname)
         dest-index-html-file (io/as-file (str dest-dir (file-separator) "index." extension))
         final-content        (render-content-if-needed content)]
     (log/infof "Write HTML to file %s" dest-file)
     (when (and final-content dest-file)
       (file-utils/ensure-out-dir (str to))
       (spit dest-file final-content))
     (log/infof "Write HTML as index.html in directory %s" dest-dir )
     (when (and final-content dest-dir)
                                        ;(file-utils/create-dirs! dest-dir)
       (file-utils/ensure-out-dir dest-dir)
       ;(println "spit" dest-dir dest-index-html-file (exists? dest-dir))
       (spit dest-index-html-file final-content))
     [dest-file dest-index-html-file])))

(defn add-docname-if-missing [x]
  (if (vector? x)
    ;;the docname is the namespace last name
    {(ns-last-name *ns*) x}
    (if (map? x)
      x
      (if (string? x)
        {(ns-last-name *ns*) x}
        (throw (ex-info (format "Render function must return either hiccup or a map of docname to hiccup, it returns %s" (type x)) {:type (type x) :x x}))))))

(defn build-render!
  "When invoked in a ns, it invokes the 'render' function that must return either hicupp or a map of docname to hiccup, then converts hiccup to html and write it"
  [& {:keys [to render-fn] :or {to PUBLIC_DIR render-fn "render"} :as params}]
  (assert-fn-present? (:ns params) "render")
  (let [render-fn              (resolve (symbol (str (:ns params)) render-fn))
        [docname hiccup]       (first (add-docname-if-missing (render-fn)))]
    ;(println *ns* to render-fn docname)
    {:render {render-fn (write-content! to "html" docname hiccup)}
     :assets (copy-assets! params)}))

(defmacro emit-build! [params]
  `(do (defn ~(symbol BUILD_FN_NAME) []
         (let [files# (doall (build-render! (assoc ~params :ns ~*ns*)))]
           (staticly/reload-browser!)
           files#))
       (staticly/register-build-function! (var ~(symbol BUILD_FN_NAME)))))

(defmacro start-server [to]
  `(let [port# ~(server/next-available-port 8080)]
     (def ~(symbol "server") {:port port# :stop-fn! (server/start-server! ~to port#)})
     (safari/open-or-reload! (str "http://localhost:" port#))))

(defmacro def-render-builder
  ([] `(def-render-builder {:from [~PUBLIC_DIR] :to ~WRITE_DIR :render-fn "render"}))
  ([{:keys [to render-fn] :or {to WRITE_DIR render-fn "render"} :as params}]
   (log/infof "Def Staticly builder: rendering function \"%s\" writing HTML to %s" render-fn to)
   `(do
      (require 'environ.core)
      (emit-build! ~(assoc params :to to :render-fn render-fn))
      (emit-main  ~(assoc params :to to :render-fn render-fn))
      (def ~(symbol "write-dir") ~to)
      (def ~(symbol "outputs") (~(symbol (str *ns*) BUILD_FN_NAME)))
                                        ;watch the clj file
      (when (developer-environment?)
        (watcher/start-watcher! ~(current-file) ~(symbol (str *ns*) BUILD_FN_NAME))
        (start-server ~to))
      ~(symbol "outputs"))))

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

(def title                           "Blog")
(def baseurl                         "http://localhost:8080")
(def description                     "Staticly blog")
(defn make-render-rss-fn [params]
  (fn render-rss-fn [markdowns] (defsquare.rss/render-rss params markdowns)))
;(def render-rss-fn (make-render-rss-fn))

(defmacro macro-default-blog-params [{:keys [ns] :as provided-params}]
  (let [{:keys [project-name doc-name]} (execution-context)
        export-rss?                     (or (:export-rss? provided-params) true)
        baseurl                         (or (:baseurl provided-params) baseurl)
        title                           (or (:title provided-params) title)
        description                     (or (:description provided-params) description)]
    (assert-fn-present? ns "post-template")
    (assert-fn-present? ns "home-template")
    (assert-fn-present? ns "tag-template")
    (assert-fn-present? ns "author-template")
    `{:from        [~doc-name ~PUBLIC_DIR]
      :to          ~(str WRITE_DIR "/" doc-name)
      :baseurl     ~baseurl
      :title       ~title
      :description ~description
      :export-rss? ~export-rss?
      :reload-word ~project-name
      :templates   {:1-1 [{:template-fn-name "post-template" :excludes nil :includes [#"\\*.md$"] :extension "html"}]
                    :n-1 [~(when export-rss? `{:template-fn (make-render-rss-fn {:baseurl ~baseurl :title ~title :description ~description}) :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "rss" :extension "xml"})
                          {:template-fn-name "home-template" :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "home" :extension "html"}
                          {:template-fn-name "tag-template" :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "tags" :extension "html" :aggregation-fn ~defsquare.staticly/aggregate-by-tags :prefix "tags"   }
                          {:template-fn-name "author-template" :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "authors" :extension "html" :aggregation-fn ~defsquare.staticly/aggregate-by-authors :prefix "authors"}]}}))

(defn default-blog-params [provided-params]
  (let [{:keys [project-name doc-name]} (execution-context)
        export-rss? (or (:export-rss? provided-params) true)
        baseurl     (or (:baseurl provided-params) baseurl)
        title       (or (:description provided-params) title)
        description (or (:description provided-params) description)]
    {:from        ["blog" PUBLIC_DIR]
     :to          (str WRITE_DIR "/" doc-name)
     :baseurl     baseurl
     :title       title
     :description description
     :export-rss? export-rss?
     :reload-word project-name
     :templates   {:1-1 [{:template-fn-name "post-template" :excludes nil :includes [#"\\*.md$"] :extension "html"}]
                   :n-1 [(when export-rss? {:template-fn (make-render-rss-fn {:baseurl baseurl :title title :description description}) :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "rss" :extension "xml"})
                         {:template-fn-name "home-template"   :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "home"    :extension "html"}
                         {:template-fn-name "tag-template"    :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "tags"    :extension "html" :aggregation-fn aggregate-by-tags    :prefix "tags"   }
                         {:template-fn-name "author-template" :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "authors" :extension "html" :aggregation-fn aggregate-by-authors :prefix "authors"}]}}))

(defn blog-params [{:keys [from to templates export-rss? reload-word ns baseurl title description] :as provided-params}]
  (let [default (default-blog-params provided-params)]
    (assoc default
           :ns          ns
           :from        (or from  (:from default))
           :to          (or (if (symbol? to) (str (var-get (resolve to))) to)  (:to default))
           :baseurl     (or baseurl (:baseurl default))
           :title       (or title (:description default))
           :description (or description (:description default))
           :templates   (or templates (:templates default))
           :export-rss? (or export-rss? (:export-rss? default))
           :reload-word (or reload-word  (:reload-word default)) )))

(defmacro macro-blog-params [{:keys [from to templates export-rss? reload-word ns baseurl title description] :as provided-params}]
  ;(println "provided-params " provided-params)
  `(let [default# (macro-default-blog-params ~provided-params)]
    (assoc default#
           :ns          ~ns
           :from        (or ~from  (:from default#))
           :to          (or ~to (:to default#))
           :baseurl     (or ~baseurl (:baseurl default#))
           :title       (or ~title (:description default#))
           :description (or ~description (:description default#))
           :templates   (or ~templates (:templates default#))
           :export-rss? (or ~export-rss? (:export-rss? default#))
           :reload-word (or ~reload-word  (:reload-word default#)) )))

(defn resolve-fn
  ([fn-name]
   (resolve (symbol (str *ns*) fn-name)))
  ([ns fn-name]
   (resolve (symbol ns fn-name))))

(defn invoke-template-1-1 [template-fn files to extension]
  (into {} (map (fn [file] (let [html-files (->> file
                                                md/process-file
                                                template-fn
                                                add-docname-if-missing
                                                (write-content! to extension))]
                            [file html-files])) files)))

(defn invoke-template-n-1 [template-fn files to extension]
  (let [[html-file index-html-file-in-dir] (->> files
                                                (map md/process-file)
                                                template-fn
                                                add-docname-if-missing
                                                (write-content! to extension))]
    {html-file              files
     index-html-file-in-dir files}))

(defn invoke-aggregation-template [template-fn aggregation-value->files to extension]
  ;(println "invoke aggregation template" template-fn)
  (into {} (map (fn [[aggregation-value files]]
                  (let [html-files (->> (template-fn aggregation-value files)
                                        add-docname-if-missing
                                        (write-content! to extension))]
                    [aggregation-value html-files])) aggregation-value->files)))

(defn- extract-template-fn [ns {:keys [template-fn template-fn-name] :as template}]
  ;(println "extract template fn " ns template-fn template-fn-name)
  (if template-fn
    (do (assert (fn? template-fn) (format  "template-fn %s is not a function, type is" template-fn (type template-fn))) template-fn)
    (do (assert-fn-present? (str ns) template-fn-name)
        (resolve-fn (str ns) template-fn-name))))

(defn build-blog-templates-1-1! [{:keys [ns from to templates] :as params}]
  (reduce (fn [acc {:keys [includes excludes extension] :as template-1-1}]
            (let [template-fn      (extract-template-fn ns template-1-1)
                  files            (file-utils/list-files from includes excludes)
                  input-to-outputs (invoke-template-1-1 template-fn files to extension)]
              (merge acc input-to-outputs)) )
          {} (:1-1 templates)))

(defn build-blog-templates-n-1! [{:keys [ns from to templates] :as params}]
  (reduce (fn [acc {:keys [includes excludes aggregation-fn prefix name extension] :as template-n-1}]
            (let [template-fn  (extract-template-fn ns template-n-1)
                  outputs      (if aggregation-fn
                                 (invoke-aggregation-template template-fn (aggregation-fn (md/process-files from includes excludes)) (if prefix (str to (file-separator) prefix) to) extension)
                                 (invoke-template-n-1         template-fn (file-utils/list-files from includes excludes)             (if prefix (str to (file-separator) prefix) to) extension))]
              (assoc acc name outputs)))
          {} (:n-1 templates)))

(defn build-blog!
  "Build a blog like structure with parameters as (see default-blog-params)"
  [& params]
  (let [{:keys [from to templates ns] :as computed-params} (blog-params params)]
    (log/infof "Build blog from %s to %s with templates %s in ns %s" from to templates ns)
    {:1-1    (build-blog-templates-1-1! computed-params)
     :n-1    (build-blog-templates-n-1! computed-params)
     :assets (copy-assets! computed-params)}))

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
  ([params]
   `(let [params# (macro-blog-params ~(assoc params :ns *ns*))]
      (assert (some exists? (:from params#)) (format "The directories '%s' must exists for the page builder to find markdown files and assets in it" (:from params#)))
      (log/infof "Define Staticly Blog builder: markdowns in %s dir rendered using 1-1 %s and n-1 %s templates found in ns %s, write output to %s" (:from params#) (:1-1 (:templates params#)) (:n-1 (:templates params#)) (:ns params#) (:to params#))
      (require 'environ.core)
      (emit-blog-build params#)
      (def ~(symbol "write-dir") (:to params#))
      (when (staticly/developer-environment?)
                                        ;watch the clj file
        (def ~(symbol "outputs") (build-blog! params#))
        (watcher/start-watcher! ~(current-file) ~(symbol (str *ns*) BUILD_FN_NAME))
        ;;watch the 'from' folder md files
        (doseq [dir# (:from params#)]
          (watcher/start-watcher! dir# ~(symbol (str *ns*) BUILD_FN_NAME))))
      ~(symbol "outputs")
      )))

(def DEFAULT_PAGE_PARAMS {:from      ["pages" PUBLIC_DIR]
                          :to        WRITE_DIR
                          :templates {:1-1 [ {:includes [#"\\*.md$"] :template-fn-name "page-template" :excludes nil :extension "html"}]}})


(defn build-page! [& {:keys [from to templates] :or {from      (:from DEFAULT_PAGE_PARAMS)
                                                     to        (:to DEFAULT_PAGE_PARAMS)
                                                     templates (:templates DEFAULT_PAGE_PARAMS) } :as params}]
  ;(println "build-page!" params from to templates)
  (when (not templates) (log/info "No templates defined in params, doing nothing"))
  {:1-1 (reduce (fn [acc {:keys [template-fn-name includes excludes extension] :as template-1-1}]
             (assert-fn-present? (:ns params) template-fn-name)
             (let [files            (file-utils/list-files from includes excludes)
                   template-fn      (resolve (symbol (str (:ns params)) template-fn-name))
                   input-to-outputs (invoke-template-1-1 template-fn files to extension)]
               (merge acc input-to-outputs)) ) {} (:1-1 templates))
   :assets (copy-assets! params)})

(defmacro emit-page-build [params]
  `(do (defn ~(symbol BUILD_FN_NAME) []
         (let [written-files# (staticly/build-page! (assoc ~params :ns ~*ns*))]
           (when (staticly/developer-environment?)
             (staticly/reload-browser!))
           written-files#))
       (staticly/register-build-function! (var ~(symbol BUILD_FN_NAME)))))

(defmacro def-page-builder
  ([]
   (let [{:keys [doc-name]} (execution-context)]
     `(def-page-builder {:from [~doc-name ~PUBLIC_DIR] :to ~WRITE_DIR})))
  ([{:keys [from to templates]}]
   (let [{:keys [doc-name]} (execution-context)
         from               (or from doc-name)
         to                 (if (not (string? to)) (str (var-get (resolve to))) to)
         templates          (or templates (:templates DEFAULT_PAGE_PARAMS))
         params             {:from from :to to :templates templates}]
     (assert (some exists? from) (format "The directories '%s' must exists for the page builder to find markdown files in it" from))
     ;(println "def-page-builder" from to single-templates doc-name)
     `(do
        (require 'environ.core)
        (log/infof "Define Staticly builder for %s: rendering markdowns in \"%s\" dir using 1-1 templates mapping %s exported to dir \"%s\"" ~(str *ns*) ~from ~templates ~to)
        (emit-page-build ~params)
        (def ~(symbol "write-dir") ~to)
        (when (staticly/developer-environment?)
          ;;execute the function and bind the result to a var
          (def ~(symbol "outputs") (~(symbol (str *ns*) BUILD_FN_NAME)))
          ;;watch the clj file
          (watcher/start-watcher! ~(current-file) ~(symbol (str *ns*) BUILD_FN_NAME))
          ;;watch the folder where sits the markdown files
          ~(doseq [dir from]
            `(watcher/start-watcher! ~dir ~(symbol (str *ns*) BUILD_FN_NAME)))
          ~(symbol "outputs"))))))

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

(defn -main [& args]
  (println "Build")
  (future
    (doseq [build!-fn @build-fns]
      (build!-fn))))


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
