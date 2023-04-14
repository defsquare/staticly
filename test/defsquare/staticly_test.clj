(ns defsquare.staticly-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [hiccup.core :as hiccup]
   [defsquare.file-utils :as file-utils :refer [exists?]]
   [defsquare.markdown :as md]
   [defsquare.ns-1-1]
   [defsquare.ns-with-blog-template]
   [defsquare.ns-with-page-template]
   [defsquare.ns-with-render]
   [defsquare.staticly :as staticly]))

(deftest test-blog-builder
  (testing "define blog-builder should invoke the template"
    ;(staticly/default-blog-params)
    ;(staticly/def-blog-builder)
    ;
    ))


(def a 1)
(resolve (symbol "a"))
(staticly/assert-symbol-present? "a")
(var-get (resolve (symbol "a")))
;(var (symbol "a"))
(meta (var a))


;; render a ns with a `render `function
;;
;; render a page from a markdown file

(deftest ns-with-render-function
  (testing "A namespace with a render function without argument"
   (load-file "test/defsquare/ns_with_render.clj")
   ;(println "export filename " defsquare.ns-with-render/export-filename)
   (is (every? file-utils/exists? (flatten (get-in defsquare.ns-with-render/outputs [:render #'defsquare.ns-with-render/render]))) (str "HTML file should exist at the locations: " defsquare.ns-with-render/outputs))
   (is (= (slurp (first (get-in defsquare.ns-with-render/outputs [:render #'defsquare.ns-with-render/render]))) "<html><body><h1>amazing content</h1></body></html>") )))

(deftest ns-with-page-template
  (testing "A namespace with a template receiving markdown as input and outputing hiccup"
    (load-file "test/defsquare/ns_with_page_template.clj")
    (doseq [[input outputs] (:1-1 defsquare.ns-with-page-template/outputs)
            output outputs]
      ;(println "output" output)
      (is (file-utils/exists? input))
      (is (file-utils/exists? output))
      (is (not (file-utils/file-empty? output)))
      (let [{:keys [metadata]} (md/parse-metadata input)]
        ;(println "metadata" metadata (:title metadata))
        (is (= (slurp output) (str "<html><head><title>" (:title metadata) "</title></head><body><h1>amazing content</h1></body></html>")))))))

(deftest ns-with-blog-template
  (testing "A namespace with several templates receiving markdown as input, aggregated markdowns and outputing hiccup"
    (load-file "test/defsquare/ns_with_blog_template.clj")
    (let [{outputs-1-1 :1-1 outputs-n-1 :n-1 assets :assets } defsquare.ns-with-blog-template/outputs]
      (is (> (count assets) 0))
      (doseq [[input outputs] outputs-1-1
              output          outputs]
        (is (file-utils/exists? output))
        (is (not (file-utils/file-empty? output)))
        (let [{:keys [metadata hiccup html file path]} (md/process-file input)]
                                        ;(println "metadata" metadata hiccup)
          (is (and html file path))
          (is (= (slurp output) (str "<html><head><title>" (:title metadata) "</title></head><body>" (hiccup/html hiccup) "</body></html>")))))
      (let [home-outputs    (get outputs-n-1 "home")
            tags-outputs    (get outputs-n-1 "tags")
            authors-outputs (get outputs-n-1 "authors")
            rss-outputs     (get outputs-n-1 "rss")]
        (is (= (count home-outputs) 2))
        (doseq [[output inputs] home-outputs]
          (is (file-utils/exists? output))
          (is (not (file-utils/file-empty? output)))
          (is (= (slurp output) (hiccup/html [:html
                                              [:head [:title "Blog"]]
                                              [:body
                                               (for [{:keys [metadata]} (map md/process-file inputs)]
                                                 [:h1 (:title metadata)])]]))))
        (is (= (keys tags-outputs) (list "clojure" "markdown" "tag2" "tag1")))
        (doseq [[tag outputs] tags-outputs]
          (is (= (count outputs) 2)))
        (is (= (keys authors-outputs) (list "david-panza" "jeremie-grodziski")))
        (doseq [[tag outputs] authors-outputs]
          (is (= (count outputs) 2)))))))

#_(deftest rendering-templates
  (testing "1-1 rendering"
    (testing "one md file, template returns a map with one entry: key is the path to export html into and value hiccup, if hiccup is returned then the md filename is used for html"
      (load-file "test/defsquare/ns_1_1.clj")
      ;(println defsquare.ns-1-1/export-dir)
      (is (exists? (str defsquare.ns-1-1/export-dir "/page1.html")) (str "page1.html HTML file rendered from markdown should exist at the location " defsquare.ns-1-1/export-dir))
      (is (exists? (str defsquare.ns-1-1/export-dir "/page1/index.html")) (str "index.html HTML file rendered from markdown should exist at the location " defsquare.ns-1-1/export-dir))
      (is (= (slurp (str defsquare.ns-1-1/export-dir "/page1.html")) "<html><head><title>Markdown Test Post 1 by Jérémie Grodziski</title></head><body><h1>file</h1><p>pages/page1.md</p><h1>type</h1><p></p><h1>amazing content</h1></body></html>"))))
  (testing "1-n rendering")
  (testing "n-1 rendering")
  (testing "n-n rendering"))

#_(deftest test-build!
  (testing "build! function with basic params"
    (build! {})))
