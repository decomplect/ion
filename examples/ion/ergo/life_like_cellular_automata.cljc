(ns ion.ergo.life-like-cellular-automata
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [criterium.core :as cr]
                     [ion.ergo.core :as ergo]))


; ------------------------------------------------------------------------------
; Research & Development


; ------------------------------------------------------------------------------
; Benchmarking

(defn bench [f]
  (cr/with-progress-reporting (cr/quick-bench (f) :verbose)))

(comment

  (bench ; GOL acorn 100 gens using sparse set of vector cells: 155 ms
    #(-> (ergo/sparse-life-rule-system
           :conway-game-of-life
           ergo/vector-cell
           (ergo/pattern :acorn))
         (nth 99) count))

  (bench ; GOL acorn 100 gens using sparse set of basic cells: 170 ms
    #(-> (ergo/sparse-life-rule-system
           :conway-game-of-life
           ergo/basic-cell
           (ergo/pattern :acorn))
         (nth 99) count))

  )
