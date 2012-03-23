(ns discwar.ai
  (:import org.codehaus.jackson.JsonParseException)
  (:import clojure.contrib.condition.Condition)
  (:use compojure.core)
  (:use ring.middleware.json-params)
  (:use ring.middleware.reload)
  (:use clojure.tools.logging)
  (:require [clj-json.core :as json]))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defn gen-response [r theta]
  {:r r :th theta})

(defn opponent-str [me]
  (if (= me "player0") "player1" "player0"))

(defn find-opponent [me all]
  (let [opponent-str (opponent-str (get me "type"))]
    (first (filter (fn [x] (= opponent-str (get x "type"))) all))))

(defn get-th [q th] 
  "A wrapper to log before returning the final theta value"
  th)

(defn angle-between-points [x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        th (Math/atan2 dy dx)
        pi-over-2 (/ Math/PI 2)
        two-pi (* 2 Math/PI)]
    (cond 
      (and (> th 0) (<= th pi-over-2)) (get-th "q1" th)
      (and (> th pi-over-2) (<= th Math/PI)) (get-th "q2" th)
      (and (> th (- 0 Math/PI)) (<= th (- 0 pi-over-2))) (get-th "q3" (+ th two-pi))
      (and (> th (- 0 pi-over-2)) (<= th 0)) (get-th "q4" (+ th two-pi)))))

(defn power-up-ai [me all]
  (let [max-acc (get me "maxAcc")]
  0))

(defn aggressive-ai [me all]
  (let [max-acc (get me "maxAcc")]
    (gen-response max-acc
      (angle-between-points 
        (get me "x") 
        (get me "y") 
        (get (find-opponent me all) "x")
        (get (find-opponent me all) "y")))))

(defn ai-response [ai params]
  (let [me (get params "me")
        all (get params "all")]
    (ai me all)))

(defroutes handler
  (POST "/" {params :params}
    (json-response 
      (ai-response aggressive-ai params))))

(def app
  (-> handler
    (wrap-reload '(discwar.ai))
    (wrap-json-params)))
