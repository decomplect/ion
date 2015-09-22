(ns ion.ergo.l-system-conway
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [criterium.core :as cr]
                     [ion.ergo.core :as ergo]))


;; -----------------------------------------------------------------------------
;; Conway's Game of Life

(defn conway-gol-xf
  [cells]
  (let [birth?   #{2 3}
        survive? #{3}]
    (comp (ergo/existing? birth? survive? cells))))

(defn conway-gol
  [seed]
  (cons (set seed) (ergo/produce conway-gol-xf ergo/prep-8 #(set seed))))

(def acorn #{[70 62] [71 60] [71 62] [73 61] [74 62] [75 62] [76 62]})

(comment (nth (conway-gol acorn) 10))


;; -----------------------------------------------------------------------------
;; Research & Development

(defn neighbors-a [[x y]]
  (map vector
       ((juxt inc inc identity dec dec dec identity inc) x)
       ((juxt identity inc inc inc identity dec dec dec) y)))

(def juxt-x (juxt inc inc identity dec dec dec identity inc))

(def juxt-y (juxt identity inc inc inc identity dec dec dec))

(defn neighbors-b [[x y]]
  (map vector (juxt-x x) (juxt-y y)))

(defn get-points [max-x max-y]
  (for [x (range max-x) y (range max-y)] [x y]))

(def sample-points (get-points 100 100))

(comment
  (cr/with-progress-reporting
    (cr/quick-bench (doall (map neighbors-a sample-points)) :verbose))
  (cr/with-progress-reporting
    (cr/quick-bench (doall (map neighbors-b sample-points)) :verbose))
  )

(comment (count (map neighbors-a sample-points)))


;; -----------------------------------------------------------------------------
;; From RM Hull

;(defn stepper [neighbours birth? survive?]
;  (fn [trim-fn cells]
;    (set (for [[loc n] (frequencies (mapcat neighbours cells))
;               :when (and
;                       (if (cells loc) (survive? n) (birth? n))
;                       (trim-fn loc))]
;           loc))))
;
;(def conways-game-of-life
;  (stepper #(place neighbours %) #{3} #{2 3}))
;
;(def semi-vote
;  (stepper #(place neighbours %) #{3 5 6 7 8} #{4 6 7 8}))
;
;(def vichniac-vote
;  (stepper #(place nine-block %) #{5 6 7 8 9} #{5 6 7 8 9}))
;
;(def unstable-vichniac-vote
;  (stepper #(place nine-block %) #{4 6 7 8 9} #{4 6 7 8 9}))
;
;(def fredkin
;  (stepper #(place nine-block %) #{1 3 5 7 9} #{1 3 5 7 9}))
;
;(def circle
;  (stepper #(place neighbours %) #{3} #{1 2 4}))
