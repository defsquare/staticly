(ns defsquare.server
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]

            [org.httpkit.server :as server]
            [ring.middleware.content-type :as content-type]
            [ring.util.mime-type :as mime-type]
            [hiccup2.core :as hiccup]
            [defsquare.files :as f])
  (:import [java.net URLDecoder URLEncoder Socket InetSocketAddress]))

(defn index [dir f]
  (let [files (map (fn [file]
                     (str (.relativize dir (f/as-path file))))
                   (f/list-files (f/file f)))]
    {:body (-> [:html
                [:head
                 [:meta {:charset "UTF-8"}]
                 [:title (str "Index of `" f "`")]]
                [:body
                 [:h1 "Index of " [:code (str f)]]
                 [:ul
                  (for [child files]
                    [:li [:a {:href (URLEncoder/encode (str child))} child (when (f/directory? (f/path dir child)) "/")]])]
                 [:hr]
                 [:footer {:style {"text-align" "center"}} "Served by defsquare/server.clj"]]]
               hiccup/html
               str)}))

(defn body [path]
  (let [file                (f/file path)
        {:keys [ext]} (f/parse-path file)]
    {:body file :headers {"Content-Type" (get mime-type/default-mime-types ext)}}))

(defn start-server!
  ([] (start-server! "." 8080))
  ([dir] (start-server! dir 8080))
  ([dir port]
   (log/infof "Start HTTP Server serving directory \"%s\" at port %s" (str dir) port)
   (let [dir     (.toAbsolutePath (f/as-path dir))
         stop-fn (server/run-server
                  (content-type/wrap-content-type (fn [{:keys [uri]}]
                                                    (let [f          (f/path dir (str/replace-first (URLDecoder/decode uri) #"^/" ""))
                                                          index-file (f/path f "index.html")]
                                                      (cond
                                                        (and (f/directory? f) (f/readable? index-file)) (body index-file)
                                                        (f/directory? f) (index dir f)
                                                        (f/readable? f) (body f)
                                                        :else {:status 404 :body (str "404 Not found: file `" f "` in " dir) :headers {"Content-Type" "text/plain"}}))))
                  {:port port})]
     (log/infof "HTTP Server started serving directory \"%s\"at port %s" (str dir) port)
     stop-fn)))

(defn server-listening?
  "Check if a given host is listening on a given port in the limit of timeout-ms (default 500 ms)"
  ([] (server-listening? 8080))
  ([port] (server-listening? "0.0.0.0" port))
  ([host port]
   (server-listening? host port 500))
  ([host port timeout-ms]
   (try (let [socket (Socket.)]
          (.connect socket (InetSocketAddress. host port) timeout-ms)
          {:listening? true :host host :port port :timeout-ms timeout-ms})
        (catch Exception e
          (log/warnf (format "Host %s at port %s is not listening! (timeout was %d ms)" host port timeout-ms))
          nil))))

(defn next-available-port
  "Find the next available port not listening on 0.0.0.0"
  [start-port]
  (loop [port start-port]
    (if (server-listening? port) (recur (inc port)) port)))
