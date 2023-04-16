(ns defsquare.ns-with-blog-template
  (:require  [clojure.test :as t]
             [defsquare.file-utils :refer [tmp-dir]]
             [defsquare.staticly :as staticly]))

(defn home-template [markdowns]
  {"home"
   [:html
    [:head [:title "Blog"]]
    [:body
     (for [{:keys [metadata]} markdowns]
       [:h1 (:title metadata)])]]}
  )

(defn post-template [{:keys [metadata html hiccup raw file path type] :as markdown}]
  {(:name path)
   [:html
    [:head [:title (:title metadata)]]
    [:body
     hiccup]]})

(defn tag-template [tag markdowns]
  {tag [:html
        [:head [:title tag]]
        [:body
         (for [{:keys [metadata hiccup]} markdowns]
           [:h1 (:title metadata)])]]})

(defn author-template [author markdowns]
  {author [:html
           [:head [:title author]]
           [:body
            (for [{:keys [metadata hiccup]} markdowns]
              [:h1 (:title metadata)])]]})

(def ^:dynamic *export-dir* (tmp-dir "staticly"))

(staticly/def-blog-builder {:to *export-dir*
                            :from ["blog" "resources/public"]
                            :baseurl "https://defsquare.io"
                          ;  :templates {:n-1 [{:template-fn      defsquare.staticly/render-rss-fn     :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "rss"     :extension "xml"}
                          ;                    {:template-fn-name "home-template"   :includes [#"\\*.md$"] :excludes [#".*DRAFT.*md"] :name "home"    :extension "html"}]}
                            })

;(staticly/build-blog!)
