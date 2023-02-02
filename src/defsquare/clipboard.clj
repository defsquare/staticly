(ns defsquare.clipboard)

(defn paste []
  (-> (java.awt.Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.getContents nil)
      (.getTransferData java.awt.datatransfer.DataFlavor/stringFlavor)))

(defn copy [x]
  (let [s (if (not (string? x)) (str x) x)]
    (-> (java.awt.Toolkit/getDefaultToolkit)
        .getSystemClipboard
        (.setContents (java.awt.datatransfer.StringSelection. s) nil))
    s))
