(ns defsquare.ns-1-1
  (:require  [clojure.test :as t]
             [defsquare.files :refer [tmp-dir]]
             [defsquare.staticly :as staticly]))

(defn page-template [{:keys [path file raw type metadata hiccup] :as markdown}]
  [:html
   [:head [:title (:title metadata) " by " (:author metadata)]]
   [:body
    [:h1 "file"]
    [:p file]
    [:h1 "type"]
    [:p type]
    [:h1 "amazing content"]]])

(def ^:dynamic *export-dir* (tmp-dir "staticly"))

(staticly/def-page-builder {:to *export-dir* :from ["pages"]})

(comment (println (slurp (str *export-dir* "/page1.html"))))
