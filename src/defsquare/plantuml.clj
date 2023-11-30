(ns defsquare.plantuml
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [defsquare.watcher :as watcher]
            [defsquare.safari :as safari]
            [defsquare.files :as files])
  (:import [net.sourceforge.plantuml SourceFileReader FileFormatOption FileFormat OptionFlags]))

(.setSilentlyCompletelyIgnoreErrors (OptionFlags/getInstance) true)

(defn- generate-with-file-option! [filename file-format]
  (let [source (io/file filename)
        reader (SourceFileReader. source)
        _      (.setFileFormatOption reader (FileFormatOption. file-format))]
    (if (.hasError reader)
      :generation-error
      (.getPngFile (nth (.getGeneratedImages reader) 0)))))

(defn generate-svg! [filename]
  (generate-with-file-option! filename FileFormat/SVG))

(defn generate-png! [filename]
  (generate-with-file-option! filename FileFormat/PNG))

(defn generate-pdf! [filename]
  (generate-with-file-option! filename FileFormat/PDF))

(defn generate!
  "Generate the PNG and SVG file diagram from a plantUML *.puml file whose path is given as an argument"
  [filename]
  (log/info (format  "Generate SVG and PNG diagram files from the PlantUML source file: %s" filename))
  {:png (generate-png! filename)
   :svg (generate-svg! filename)
  ; :pdf (generate-pdf filename)
   })

(defn- generate-then-reload! [filename]
  (let [result (future (generate! filename))]
          (when @result
            (if (and (= :generation-error (:png @result)) (= :generation-error (:svg @result)))
              (log/info (format  "Error raised during generation of diagram from PlantUML file %s : " filename) (with-out-str (clojure.pprint/pprint @result)))
              (do
                (log/info (format  "Diagram generated from PlantUML file %s : " filename) (with-out-str (clojure.pprint/pprint @result)))
                (safari/reload-tab! (files/name filename)))))))

(defn watch!
  "Generate and reload the safari tab opened on the SVG file
   then watch the PlantUML *.puml file (filename arg) and trigger a generation everytime the file is touched"
  [filename]
  (generate-then-reload! filename)
  (watcher/start-watcher! filename (partial generate-then-reload! filename)))

