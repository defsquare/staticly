(ns defsquare.hiccup
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk :refer [postwalk]]
            [hickory.core :as hickory :refer [as-hiccup parse-fragment]]))

(defn- replace-nil-with-blank [v]
  (map (fn [x] (if x x "")) v))

(defn- remove-empty-form [x]
  (if (and (string? x) (clojure.string/blank? x))
    nil
    (if (vector? x)
      (if (= 2 (count x))
        (vec (replace-nil-with-blank x))
        (vec (filter remove-empty-form x)))
      (if (and (map? x) (empty? x))
        nil
        x))))

(defn clean-hiccup [hiccup]
  (walk/postwalk remove-empty-form hiccup))

(defn html-str->hiccup [html-str]
  (->> html-str
       hickory/parse-fragment
       (map hickory/as-hiccup)
       clean-hiccup))

(defn html-file->hiccup [f]
  (html-str->hiccup (slurp f)))

(defn html->hiccup [html]
  (let [f (io/file html)
        s (if (.exists f)
            (slurp f)
            html)]
    (html-str->hiccup s)))
