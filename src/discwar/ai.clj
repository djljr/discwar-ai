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
  (info (str "r: " r ", theta: " theta))
  {:r r :th theta})

(defn find-opponent [all]
  (first (filter (fn [x] (= "player0" (get x "type"))) all)))

(defn get-th [q th]
  (info q)
  (- th Math/PI))

(defn angle-between-points [x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        th (+ Math/PI (Math/atan2 dx dy))
        pi-over-2 (/ Math/PI 2)
        three-pi-over-2 (* 3 pi-over-2)]
    (info "theta: " th)
    (cond 
      (and (> th 0) (<= th pi-over-2)) (get-th "q1" th)
      (and (> th pi-over-2) (<= th Math/PI)) (get-th "q2" (- th Math/PI))
      (and (> th Math/PI) (<= th three-pi-over-2)) (get-th "q3" th)
      (> th three-pi-over-2) (get-th "q4" (- th Math/PI)))))

(defn aggressive-ai [me all]
  (gen-response 0.8 
    (angle-between-points 
      (get me "x") 
      (get me "y") 
      (get (find-opponent all) "x")
      (get (find-opponent all) "y"))))

(defroutes handler
  (POST "/" {params :params}
    (json-response 
      (aggressive-ai 
        (get params "me") 
        (get params "all")))))

(def app
  (-> handler
    (wrap-reload '(discwar.ai))
    (wrap-json-params)))
