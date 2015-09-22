(ns ion.ergo.l-system-conway
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [criterium.core :as cr]
                     [ion.ergo.l-system :as ls]))


;; -----------------------------------------------------------------------------
;; Cellular Automata

(defn ca-produce
  "Returns a lazy sequence of cells."
  [prep get-xf seed]
  (letfn [(process [cells]
                   (lazy-seq
                     (when (seq cells)
                       (let [cells (into #{} (get-xf cells) (prep cells))]
                         (cons cells (process cells))))))]
    (process seed)))

(defn neighbors-4 [[x y]]
  (map vector
       ((juxt inc identity dec identity) x)
       ((juxt identity inc identity dec) y)))

(defn neighbors-5 [[x y]]
  (map vector
       ((juxt inc identity dec identity identity) x)
       ((juxt identity inc identity dec identity) y)))

(defn neighbors-8 [[x y]]
  (map vector
       ((juxt inc inc identity dec dec dec identity inc) x)
       ((juxt identity inc inc inc identity dec dec dec) y)))

(defn neighbors-9 [[x y]]
  (map vector
       ((juxt inc inc identity dec dec dec identity inc identity) x)
       ((juxt identity inc inc inc identity dec dec dec identity) y)))

(defn prep-8
  "Returns a map of [x y] n pairs."
  [cells]
  (frequencies (mapcat neighbors-8 cells)))

(defn exist?
  "Returns a cell if destiny will allow, or mother nature brings it to life."
  [birth? survive? cells [cell n]]
  (if (cells cell)
    (when (survive? n) cell)
    (when (birth? n) cell)))

(defn existing?
  "Returns an existence-determining transducer."
  [birth? survive? cells]
  (keep (partial exist? birth? survive? cells)))


;; -----------------------------------------------------------------------------
;; Conway's Game of Life

(defn conway-gol-xf
  [cells]
  (let [birth?   #{2 3}
        survive? #{3}]
    (comp (existing? birth? survive? cells))))

(defn conway-gol
  [seed]
  (ca-produce prep-8 conway-gol-xf seed))

(def acorn #{[70 62] [71 60] [71 62] [73 61] [74 62] [75 62] [76 62]})

(comment (nth (conway-gol acorn) 2))


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
