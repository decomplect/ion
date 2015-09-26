(ns ion.ergo.l-system-conway
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [criterium.core :as cr]
                     [ion.ergo.core :as ergo]))


;; -----------------------------------------------------------------------------
;; Research & Development

(def neighborhood-8-x (juxt inc inc identity dec dec dec identity inc))
(def neighborhood-8-y (juxt identity inc inc inc identity dec dec dec))

(defn neighborhood-8 [[x y]]
  (map vector (neighborhood-8-x x) (neighborhood-8-y y)))

(defn neighbor-freq-8
  "Returns a map of [x y] neighbor-count pairs."
  [cells]
  (frequencies (mapcat neighborhood-8 cells)))

(defn produce
  "Returns a lazy sequence of colls from a recursive, axiomatic, transducible
   process."
  [seed prep-f get-xf]
  (letfn [(process
            [coll]
            (lazy-seq
              (when (seq coll)
                (let [new-coll (into (empty coll) (get-xf coll) (prep-f coll))]
                  (cons new-coll (process new-coll))))))]
    (process seed)))

(defn ca-system
  [seed prep-f get-xf]
  (cons (set seed) (produce (set seed) prep-f get-xf)))

(defn exist
  "Returns a cell if destiny will allow, or mother nature brings it to life."
  [survive? birth? cell-maker-f cells [cell-position neighbor-count]]
  (if (cells cell-position)
    (when (survive? neighbor-count) (cell-maker-f cell-position))
    (when (birth? neighbor-count) (cell-maker-f cell-position))))

(defn existing
  "Returns an existence-determining transducer."
  [survive? birth? cell-maker-f cells]
  (keep (partial exist survive? birth? cell-maker-f cells)))

(defn ca-xf
  [survive? birth? cell-maker-f cells]
  (existing survive? birth? cell-maker-f cells))

(defn ca-sequence
  [survive? birth? neighbor-freq-f cell-maker-f seed]
  (let [seed (into #{} (map cell-maker-f) seed)
        prep-f neighbor-freq-f
        get-xf (partial ca-xf survive? birth? cell-maker-f)]
    (ca-system seed prep-f get-xf)))

(defn ca-builder
  [rule-key cell-maker-f seed]
  (let [[survive? birth? neighbor-freq-f] (ergo/ca-rules rule-key)
        freq-f neighbor-freq-8]
    (ca-sequence survive? birth? freq-f cell-maker-f seed)))

(defn get-points [max-x max-y]
  (for [x (range max-x) y (range max-y)] [x y]))

(def sample-points (get-points 100 100))

(comment

  (cr/with-progress-reporting
    (cr/quick-bench ; Local Research & Development
      (count (-> (ca-builder :conway-game-of-life identity
                             (ergo/pattern :acorn)) (nth 100))) :verbose))

  (cr/with-progress-reporting
    (cr/quick-bench ; Production Version
      (count (-> (ergo/ca-builder :conway-game-of-life identity
                                  (ergo/pattern :acorn)) (nth 100))) :verbose))

  (cr/with-progress-reporting
    (cr/quick-bench
      (count (-> (ergo/ca-builder :conway-game-of-life ergo/cell
                                  (ergo/pattern :acorn)) (nth 100))) :verbose))

  (cr/with-progress-reporting
    (cr/quick-bench (doall (map neighborhood-8 sample-points)) :verbose))

  (cr/with-progress-reporting
    (cr/quick-bench (doall (map neighborhood-8 sample-points)) :verbose))
  )

(comment (count (map neighborhood-8-a sample-points)))

