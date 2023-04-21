(ns defsquare.ns-with-page-template
  (:require  [clojure.test :as t]
             [defsquare.files :refer [tmp-dir]]
             [defsquare.staticly :as staticly]))

(defn page-template [{:keys [metadata html hiccup raw file path type] :as markdown}]
  {(:name path)
   [:html
    [:head [:title (:title metadata)]]
    [:body
     [:h1 "amazing content"]]]})

(def ^:dynamic *export-dir* (tmp-dir "staticly"))

(staticly/def-page-builder {:to *export-dir* :from ["pages"]})

;(staticly/build-page!)
