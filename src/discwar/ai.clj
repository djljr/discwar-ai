(ns discwar.ai
  (:use compojure.core)
  (:use ring.middleware.json-params)
  (:use ring.middleware.reload)
  (:require [clj-json.core :as json]))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defn gen-response [r theta]
  {:r r :th theta})

(defn aggressive-ai [me all]
  ;(prn "me: " me " all: " all " me.x: " (get me "x") " keys: " (keys me)))
  (gen-response 1 (get me "x")))

(defroutes handler
  (POST "/" {params :params}
    ;(json-response (gen-response 1 0))))
    (json-response (aggressive-ai (get params "me") (get params "all")))))

(def app
  (-> handler
    (wrap-reload '(discwar.ai))
    (wrap-json-params)))
