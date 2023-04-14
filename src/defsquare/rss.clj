(ns defsquare.rss
  (:require [clj-rss.core :as rss]
            [clojure.tools.logging :as log]
            [defsquare.file-utils :refer [file-separator]]))

(defn blog-post-link [{:keys [baseurl from] :as params} markdown]
  ;(println "blog-post-link" (:metadata markdown))
  (str baseurl "/" from "/" (get-in markdown [:path :name])))

(defn render-rss [params markdowns]
  (assert (and (contains? params :title) (contains? params :baseurl) (contains? params :description)) "Blog parameters must contains `:title`, `:baseurl` and `:description` key when rendering RSS")
  (log/info (format "Render RSS feed"))
  {"rss" (rss/channel-xml {:title       (:title params)
                           :link        (str (:baseurl params) "/" (:from params))
                           :feed-url    (str (:baseurl params) "/" (:from params) "/rss.xml")
                           :description (:description params)}
                          (filter identity (map (fn [markdown]
                                                  (let [{:keys [timestamp title author path summary] :as metadata} (:metadata markdown)]
                                        ;(println "export-rss! item" title author summary)
                                                    (when metadata
                                                      {:title       title
                                                       :author      author
                                                       :description summary
                                                       :link        (blog-post-link params markdown)}))) markdowns)))})

(defn write-rss! [params markdowns]
  ;(println "export-rss!" params markdowns)
  (let [rss-xml (render-rss params markdowns)]
    (println (format "Write RSS feed to rss.xml"))
    (spit (str (:to params) (file-separator) "rss.xml") rss-xml)))
