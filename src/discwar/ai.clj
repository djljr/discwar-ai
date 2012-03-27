(ns discwar.ai
  (:import org.codehaus.jackson.JsonParseException)
  (:import clojure.contrib.condition.Condition)
  (:use ring.adapter.jetty)
  (:use compojure.core)
  (:use ring.middleware.json-params)
  (:use ring.middleware.reload)
  (:use clojure.tools.logging)
  (:require [clj-json.core :as json])
  (:gen-class))

(defn json-response 
  "correctly set headers and transform the map provided into JSON"
  [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Methods" "POST"}
   :body (json/generate-string data)})

(defn gen-response 
  "construct the map used to report desired acceleration"
  [r theta]
  {:r r :th theta})

(defn opponent-str 
  "return the name of the other player"
  [me]
  (if (= me "player0") "player1" "player0"))

(defn dist 
  "eucledian distance between (x1,y1) and (x2,y2)"
  [x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn closest-to-center
  "returns the item in coll which is closest to (x-center,y-center)"
  [coll x-center y-center]
  (first (sort (fn [o1 o2] (- (get o1 "dist") (get o2 "dist")))
    (map (fn [o] (assoc o "dist" (dist(get o "x") (get o "y") x-center y-center))) coll))))

(defn filter-type 
  "filter coll to things with the given type and return the one closest to (x-center,y-center)"
  [type coll x-center y-center]
  (closest-to-center (filter (fn [x] (= type (get x "type"))) coll) x-center y-center))

(defn find-opponent 
  "find the object representing the other player"
  [me all x-center y-center]
  (let [opponent-str (opponent-str (get me "type"))]
    (filter-type opponent-str all x-center y-center)))

(defn find-powerup 
  "find the powerup closest to (x-center,y-center)"
  [all x-center y-center]
  (filter-type "powerup" all x-center y-center))

(defn is-inside 
  "true if me is closer to center than opp"
  [me opp x-center y-center]
  (let [x-me (get me "x")
        y-me (get me "y")
        x-opp (get opp "x")
        y-opp (get opp "y")
        dist-me (dist x-me y-me x-center y-center)
        dist-opp (dist x-opp y-opp x-center y-center)]
    (debug "center: " x-center ", " y-center "; dist-me: " dist-me ", dist-opp: " dist-opp)
    (< dist-me dist-opp)))
      

(defn find-mass-diff 
  "difference in mass between me and opp"
  [me opp]
  (let [my-mass (get me "mass")
       opp-mass (get opp "mass")]
    (- my-mass opp-mass)))

(defn get-th 
  "A wrapper to log before returning the final theta value"
  [q th] 
  th)

(defn fast-move-to-goal 
  "change the move target based on some delta"
  [th-goal th-delta]
  (debug "using fast fn")
  (+ th-goal th-delta))

(defn compute-acceleration-theta [a-max th-current th-goal]
  "decide how to accelerate such that the new velocity direction is close the th-goal"
  (let [th-delta (- th-goal th-current)
        pi-over-2 (/ Math/PI 2)]
    (debug "th-delta: " th-delta ", th-goal: " th-goal ", th-current: " th-current)
    (if 
      (or (> th-delta pi-over-2) (< th-delta (- 0 pi-over-2))) th-goal
      (fast-move-to-goal th-goal th-delta))))

(defn red []
  (debug "red zone occupied")
  :red)

(defn compute-zone
  "decide which zone `obj` resides in (`:red`, `:yellow`, `:green`)" 
  [obj settings]
  (let [x-obj (get obj "x")
        y-obj (get obj "y")
        x-center (/ (get settings "maxWidth") 2)
        y-center (/ (get settings "maxHeight") 2)
        radius (get settings "boardRadius")
        dist-obj (dist x-obj y-obj x-center y-center)]
    (cond
      (> dist-obj (* radius 0.8)) (red)
      (> dist-obj (* radius 0.7)) :yellow
      (> dist-obj 0) :green)))

(defn angle-between-points 
  "compute the angle between the points (x1,y1) and (x2,y2) in the range of 0 to 2pi"
  [x1 y1 x2 y2]
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

(defn center-ai 
  "ai who will always aim towards the center of the board"
  [me all settings]
  (let [max-acc (get me "maxAcc")
        v-current (get me "v")
        v-th-current (get v-current "th")
        x-me (get me "x")
        y-me (get me "y")
        x-center (/ (get settings "maxWidth") 2) 
        y-center (/ (get settings "maxHeight") 2)
        th-center (angle-between-points x-me y-me x-center y-center)]
    (gen-response max-acc (compute-acceleration-theta max-acc v-th-current th-center))))

(defn powerup-ai 
  "ai who will aim for the powerup nearest the center of the board"
  [me all settings]
  (let [max-acc (get me "maxAcc")
        x-center (/ (get settings "maxWidth") 2) 
        y-center (/ (get settings "maxHeight") 2)
        powerup (find-powerup all x-center y-center)
        v-current (get me "v")
        v-th-current (get v-current "th")
        x-me (get me "x")
        y-me (get me "y")
        x-goal (get powerup "x")
        y-goal (get powerup "y")
        th-goal (angle-between-points x-me y-me x-goal y-goal)]
    (gen-response max-acc (compute-acceleration-theta max-acc v-th-current th-goal))))

(defn aggressive-ai 
  "ai who will always aim towards the opponent" 
  [me all settings]
  (let [max-acc (get me "maxAcc")
        x-center (/ (get settings "maxWidth") 2) 
        y-center (/ (get settings "maxHeight") 2)
        opponent (find-opponent me all x-center y-center)
        v-current (get me "v")
        v-th-current (get v-current "th")
        x-me (get me "x")
        y-me (get me "y")
        x-goal (get opponent "x")
        y-goal (get opponent "y")
        th-goal (angle-between-points x-me y-me x-goal y-goal)]
    (gen-response max-acc (compute-acceleration-theta max-acc v-th-current th-goal))))

(defn aggressive-ai-when-hes-on-the-ropes [me all settings]
  (debug "choosing aggressive-ai because we have them on the ropes")
  (aggressive-ai me all settings))

(defn center-ai-when-in-yellow-zone [me all settings]
  (debug "choosing center-ai due to yellow or red zone infraction while outside of opponent")
  (center-ai me all settings))

(defn center-ai-when-behind-in-power [me all settings]
  (debug "choosing center-ai due to power difference")
  (center-ai me all settings))

(defn powerup-ai-when-in-range [me all settings]
  (debug "choosing powerup-ai since there is one in range")
  (powerup-ai me all settings))

(defn choose-ai 
  "decide which mode to operate in based on the following logic:
   * if the opponent is outside of us in the yellow zone be aggressive
   * if a powerup is on the battlefield and that powerup is not in the
     red zone: go after it unless we have a lead greater than 1.0 in mass
     unless both players are in the green zone (meant to counteract center
     holding AIs)
   * if we are on the outsize and not in the green zone make a b-line for
     the center
   * otherwise be aggressive"
  [me all settings]
  (let [max-acc (get me "maxAcc")
        x-center (/ (get settings "maxWidth") 2) 
        y-center (/ (get settings "maxHeight") 2)
        opponent (find-opponent me all x-center y-center)
        powerup (find-powerup all x-center y-center)
        mass-diff (find-mass-diff me opponent)
        zone-me (compute-zone me settings)
        zone-opponent (compute-zone opponent settings)
        me-inside (is-inside me opponent x-center y-center)]
    (cond
      (and (= :yellow zone-opponent) me-inside) (aggressive-ai-when-hes-on-the-ropes me all settings)
      (and (not (= nil powerup)) (not (= :red (compute-zone powerup settings))) 
        (or (< mass-diff 1) (and (= :green zone-me) (= :green zone-opponent)))) (powerup-ai-when-in-range me all settings)
      (< mass-diff -0.6) (center-ai-when-behind-in-power me all settings)
      (and (not (= :green zone-me)) (not me-inside)) (center-ai-when-in-yellow-zone me all settings)
      (and) (aggressive-ai me all settings))))

(defn ai-response 
  "wrapper around calling an AI with the web request params to get
   the common values out of the map"
  [ai params]
  (let [me (get params "me")
        all (get params "all")
        settings (get params "settings")]
    (ai me all settings)))

(defroutes handler
  (POST "/" {params :params}
    (json-response 
      (ai-response choose-ai params)))
  (ANY "/" {params :params}
    (json-response
      (gen-response 0 0))))

(defn app
  []
  (-> handler
    (wrap-reload '(discwar.ai))
    (wrap-json-params)))

(defn -main [& args]
  (run-jetty (app) {:port 9090}))
