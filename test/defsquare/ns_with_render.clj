(ns defsquare.ns-with-render
  (:require  [defsquare.staticly :as staticly]
             [defsquare.files :refer [tmp-dir]]))

(defn render []
  ;(println "render")
  [:html
   [:body
    [:h1 "amazing content"]]])

(def ^:dynamic *export-dir* (tmp-dir "staticly"))

(staticly/def-render-builder {:to *export-dir*})

