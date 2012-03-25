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

(defn filter-type [type coll]
  (first (filter (fn [x] (= type (get x "type"))) coll)))

(defn find-opponent [me all]
  (let [opponent-str (opponent-str (get me "type"))]
    (filter-type opponent-str all)))

(defn find-powerup [all]
  (filter-type "powerup" all))

(defn find-mass-diff [me opp]
  (let [my-mass (get me "mass")
       opp-mass (get opp "mass")]
    (- my-mass opp-mass)))

(defn get-th [q th] 
  "A wrapper to log before returning the final theta value"
  th)

(defn compute-acceleration-theta [a-max th-current th-goal]
  (let [th-delta (- th-goal th-current)
        pi-over-2 (/ Math/PI 2)]
    (if 
      (or (> th-delta pi-over-2) (< th-delta pi-over-2)) th-goal
      (+ th-goal th-delta))))

(defn compute-zone [obj settings]
  (let [x-obj (get obj "x")
        y-obj (get obj "y")
        x-center (/ (get settings "maxWidth") 2)
        y-center (/ (get settings "maxHeight") 2)
        radius (get settings "boardRadius")
        dx (- x-obj x-center)
        dy (- y-obj y-center)
        dist-obj (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (cond
      (> dist-obj (* radius 0.9)) :red
      (> dist-obj (* radius 0.8)) :yellow
      (> dist-obj 0) :green)))

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

(defn choose-ai [me all settings]
  (let [max-acc (get me "maxAcc")
        opponent (find-opponent me all)
        powerup (find-powerup all)
        mass-diff (find-mass-diff me opponent)
        zone-me (compute-zone me settings)
        zone-opponent (compute-zone opponent settings)]
    (cond
      (and (not (= nil powerup)) (= :green (compute-zone powerup settings)) 
        (or (< mass-diff 1) (and (= :green zone-me) (= :green zone-opponent)))) (powerup-ai me all settings)
      (< mass-diff -0.6) (center-ai me all settings)
      (and (= :red zone-me) (not (= :red zone-opponent))) (center-ai me all settings)
      (and) (aggressive-ai me all settings))))

(defn center-ai [me all settings]
  (let [max-acc (get me "maxAcc")
        v-current (get me "v")
        v-th-current (get v-current "th")
        x-me (get me "x")
        y-me (get me "y")
        x-center (/ (get settings "maxWidth") 2) 
        y-center (/ (get settings "maxHeight") 2)
        th-center (angle-between-points x-me y-me x-center y-center)]
    (gen-response max-acc (compute-acceleration-theta max-acc v-th-current th-center))))

(defn powerup-ai [me all settings]
  (let [max-acc (get me "maxAcc")
        powerup (find-powerup all)
        v-current (get me "v")
        v-th-current (get v-current "th")
        x-me (get me "x")
        y-me (get me "y")
        x-goal (get powerup "x")
        y-goal (get powerup "y")
        th-goal (angle-between-points x-me y-me x-goal y-goal)]
    (gen-response max-acc (compute-acceleration-theta max-acc v-th-current th-goal))))

(defn aggressive-ai [me all settings]
  (let [max-acc (get me "maxAcc")
        opponent (find-opponent me all)
        v-current (get me "v")
        v-th-current (get v-current "th")
        x-me (get me "x")
        y-me (get me "y")
        x-goal (get opponent "x")
        y-goal (get opponent "y")
        th-goal (angle-between-points x-me y-me x-goal y-goal)]
    (gen-response max-acc (compute-acceleration-theta max-acc v-th-current th-goal))))

(defn ai-response [ai params]
  (let [me (get params "me")
        all (get params "all")
        settings (get params "settings")]
    (ai me all settings)))

(defroutes handler
  (POST "/" {params :params}
    (json-response 
      (ai-response choose-ai params))))

(def app
  (-> handler
    (wrap-reload '(discwar.ai))
    (wrap-json-params)))
