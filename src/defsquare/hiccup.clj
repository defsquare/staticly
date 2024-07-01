(ns defsquare.hiccup
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk :refer [postwalk]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [hickory.core :as hickory :refer [as-hiccup parse-fragment]]
            [defsquare.clipboard :as clipboard]
            [defsquare.markdown :as md]
            ))

(defn paste []
  (-> (java.awt.Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.getContents nil)
      (.getTransferData java.awt.datatransfer.DataFlavor/stringFlavor)))

(defn copy [s]
  (-> (java.awt.Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.setContents (java.awt.datatransfer.StringSelection. s) nil)))

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
        (if (nil? x)
          nil
          (if (seq? x)
            (if (= 2 (count x))
              (vec (replace-nil-with-blank x))
              (vec (filter remove-empty-form x)))
            x))))))

(defn clean-hiccup [hiccup]
  (walk/postwalk remove-empty-form hiccup))


(defn html-str->hiccup [html-str]
  (->> html-str
       hickory/parse-fragment
       (map hickory/as-hiccup)
       clean-hiccup))

(defn md->hiccup [md]
  (html-str->hiccup (md/md->html md)))

(defn md-fragment->hiccup [md]
  (when md
    (vec (concat [:div] (md->hiccup md)))))

(defn html-file->hiccup [f]
  (html-str->hiccup (slurp f)))

(defn html->hiccup [html]
  (let [f (io/file html)
        s (if (.exists f)
            (slurp f)
            html)]
    (html-str->hiccup s)))

(defn clipboard->hiccup []
  (binding [clojure.pprint/*print-right-margin* 160]
    (clipboard/copy (with-out-str
                      (-> (clipboard/paste)
                          html->hiccup
                          pprint)))))
