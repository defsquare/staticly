 (ns defsquare.plantuml
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [defsquare.watcher :as watcher]
            [defsquare.safari :as safari]
            [defsquare.files :as files])
  (:import [net.sourceforge.plantuml SourceFileReader FileFormatOption FileFormat OptionFlags GeneratedImage]
           [java.io File ByteArrayOutputStream]
           [java.nio.file Path Paths])
  (:gen-class))

;(set! *warn-on-reflection* true)
(.setSilentlyCompletelyIgnoreErrors (OptionFlags/getInstance) true)

;;Use SourceStringReader instead of SourceFileReader (see https://javadoc.io/static/net.sourceforge.plantuml/plantuml/8038/net/sourceforge/plantuml/SourceStringReader.html )
#_(defn- generate-graphviz! [filename]
  (let [^File source (io/file filename)
        ^SourceFileReader reader (SourceFileReader. source)
        ^ByteArrayOutputStream os (java.io.ByteArrayOutputStream.)]
    (if (.hasError reader)
      :generation-error
      (do (.generateDiagramDescription ^SourceFileReader reader ^ByteArrayOutputStream os 0 ^"[Ljava.lang.String" (into-array String ["-tsvg"]))
          (String. (.toByteArray os)) )
      )))

(defn- generate-with-file-option! [filename file-format base-directory output-directory]
  (let [^File source (io/file filename)
        ^String output-directory-path (case
                                  (instance? File output-directory) (.toAbsolutePath output-directory)
                                  (instance? String output-directory) output-directory)
        ^File relativized-output-directory (when (not-empty output-directory) (.toFile ^Path (files/path output-directory-path (.toString ^Path (files/relativize base-directory (.getParentFile source))))))
        ^SourceFileReader reader (if (not-empty output-directory) (SourceFileReader. source relativized-output-directory) (SourceFileReader. source))
        ^FileFormatOption fileFormatOption (FileFormatOption. file-format)]
    (.setDebugSvek fileFormatOption true)
    (.setFileFormatOption reader fileFormatOption)
    (if (.hasError reader)
      :generation-error
      (str (.getPngFile ^GeneratedImage (nth (.getGeneratedImages reader) 0))))))

(defn generate-svg! [filename base-directory output-directory]
  (generate-with-file-option! filename FileFormat/SVG base-directory output-directory))

(defn generate-png! [filename base-directory output-directory]
  (generate-with-file-option! filename FileFormat/PNG base-directory output-directory))

(defn generate-pdf! [filename base-directory output-directory]
  (generate-with-file-option! filename FileFormat/PDF base-directory output-directory))

(defn generate!
  "Generate the PNG and SVG file diagram from a plantUML *.puml file whose path is given as an argument"
  [filename & {:keys [formats output-directory base-directory] :as options :or {formats #{:svg :png}}}]
  ;(println "options" options)
  (files/assert-exists? filename)
  (println (format  "Generate SVG and PNG diagram files from the PlantUML source file: %s" filename))
  (cond-> {}
    (contains? formats :png) (assoc :png (generate-png! filename base-directory output-directory))
    (contains? formats :svg) (assoc :svg (generate-svg! filename base-directory output-directory))))

(defn- generate-then-reload! [filename options]
  (println "Generate PlantUML file" filename)
  (let [result (future (generate! filename options))]
          (when @result
            (if (and (= :generation-error (:png @result)) (= :generation-error (:svg @result)))
              (println (format  "Error raised during generation of diagram from PlantUML file %s : " filename) (with-out-str (clojure.pprint/pprint @result)))
              (do
                (println (format  "Diagram generated from PlantUML file %s : " filename) (with-out-str (clojure.pprint/pprint @result)))
                (safari/reload-tab! (files/name filename)))))))

(defn watch!
  "Generate and reload the safari tab opened on the SVG file
   then watch the PlantUML *.puml file (filename arg) and trigger a generation everytime the file is touched"
  [filename & {:keys [formats output-directory base-directory] :as options :or {formats #{:svg :png}}}]
  (println "watch PlantUML file " filename options)
  ;(generate-then-reload! filename options)
  (watcher/start-watcher! filename (partial generate-then-reload! filename options)))

(defn watch-dir!
  ([]
   (watch-dir! (files/current-working-directory)))
  ([dir]
   (watch-dir! dir "glob:**/*.puml" {:formats #{:svg :png} :output-directory "."}))
  ([^File dir ^String glob &{:keys [formats output-directory] :as options :or {formats #{:svg :png} output-directory "."}} ]
   (println "PlantUML watcher: watch dir" (str dir) glob options)
   (println "Watched files:" (files/glob-file-seq glob dir) )
   (dorun (for [^File file (files/glob-file-seq glob dir)]
            (let [filename (.getAbsolutePath file)
                  options  (update options :output-directory (fn [f] (.getAbsolutePath (io/file dir f))))]
              (watch! filename (assoc options :base-directory dir)))))))

(defn reset! []
  (watcher/reset-watcher!))

(defn -main [& args]
  (future (watch-dir!))
  )
