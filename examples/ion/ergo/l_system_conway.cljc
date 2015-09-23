(ns ion.ergo.l-system-conway
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [criterium.core :as cr]
                     [ion.ergo.core :as ergo]))


;; -----------------------------------------------------------------------------
;; Conway's Game of Life

(def acorn #{[70 62] [71 60] [71 62] [73 61] [74 62] [75 62] [76 62]})

(comment (nth (ergo/ca-conway acorn) 10))


;; -----------------------------------------------------------------------------
;; Research & Development

(defn neighborhood-8-a [[x y]]
  (map vector
       ((juxt inc inc identity dec dec dec identity inc) x)
       ((juxt identity inc inc inc identity dec dec dec) y)))

(def compass-8-x (juxt inc inc identity dec dec dec identity inc))

(def compass-8-y (juxt identity inc inc inc identity dec dec dec))

(defn neighborhood-8-b [[x y]]
  (map vector (compass-8-x x) (compass-8-y y)))

(defn get-points [max-x max-y]
  (for [x (range max-x) y (range max-y)] [x y]))

(def sample-points (get-points 100 100))

(comment
  (cr/with-progress-reporting
    (cr/quick-bench (doall (map neighborhood-8-a sample-points)) :verbose))
  (cr/with-progress-reporting
    (cr/quick-bench (doall (map neighborhood-8-b sample-points)) :verbose))
  )

(comment (count (map neighborhood-8-a sample-points)))
