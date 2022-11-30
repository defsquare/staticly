(ns defsquare.staticly-test
  (:require [clojure.test :as test :refer [deftest testing is]]
            [defsquare.staticly :as staticly]))

(deftest test-blog-builder
  (testing "define blog-builder should invoke the template"
    (staticly/default-blog-params)
    ;(staticly/def-blog-builder)
    ;
    ))


(def a 1)
(resolve (symbol "a"))
(staticly/assert-symbol-present? "a")
(var-get (resolve (symbol "a")))
(var (symbol "a"))
(meta (var a))


a
