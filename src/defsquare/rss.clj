(ns defsquare.rss
  (:require [clj-rss.core :as rss]
            [defsquare.file-utils :refer [file-separator]]))

(defn blog-post-link [{:keys [baseurl from] :as params} markdown]
  (str baseurl "/" from "/" (get-in markdown [:path :name])))

(defn export-rss! [params markdowns]
  ;(println "export-rss!" params markdowns)
  (let [rss-xml (rss/channel-xml
                 {:title       (:title params)
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
                                              :link        (blog-post-link params markdown)}))) markdowns)))]
    (println (format "Export RSS feed to rss.xml"))
    (spit (str (:to params) (file-separator) "rss.xml") rss-xml)))
