(ns ion.ergo.l-system-examples
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [criterium.core :as cr]
                     [ion.ergo.core :as ergo]))

; -----------------------------------------------------------------------------
; Helper Functions

(defn neighbors
  "Returns a vector of left / right neighbor values."
  [word index]
  [(get word (dec index)) (get word (inc index))])


; -----------------------------------------------------------------------------
; Example Systems

(defn basic-fibonacci-sequence
  "Returns a lazy sequence of vectors of Fibonacci integers - OEIS A003849."
  []
  (apply ergo/basic-rewriting-system (ergo/grammar :A003849)))

(deftest fibonacci-sequence-basic-test
  (is (= 144 (-> (basic-fibonacci-sequence) (nth 10) count))))


(defn stochastic-fibonacci-sequence
  "Returns a lazy sequence of vectors of Fibonacci integers starting randomly
   with 0 or 1."
  []
  (let [axiom #(vec [(rand-int 2)])
        rules {0 [0 1]
               1 [0]}]
    (ergo/functional-rewriting-system axiom rules)))


(defn generational-sequence
  "Returns a lazy sequence of integers."
  []
  (let [axiom [0]
        rules (fn [g]
                {0 [0 g 1]
                 1 []
                 2 [0]
                 3 [1 2 3 4]
                 4 []})]
    (ergo/basic-rewriting-system axiom rules)))


(defn stochastic-generational-sequence
  "Returns a lazy sequence of semi-random integers."
  []
  (let [axiom [0]
        rules (fn [g]
                {0 [0 (rand-int (+ g 5)) 1]
                 1 [0]})]
    (ergo/basic-rewriting-system axiom rules)))


(defn changing-rules-sequence
  "Returns a lazy sequence of integers."
  []
  (let [axiom [0]
        rules (fn [g]
                (merge
                  {(- g 3) []
                   (- g 2) [99]}
                  {0 [0 1 2 3]
                   1 []
                   2 [0]
                   3 [1 2 3 4]
                   4 [g]}))]
    (ergo/basic-rewriting-system axiom rules)))


(defn dragon-sequence
  "Returns a lazy sequence of vectors."
  []
  (let [axiom [:F :x]
        rules {:x [:x :+ :y :F :+]
               :y [:- :F :x :- :y]}]
    (ergo/basic-rewriting-system axiom rules)))


(comment
  (take 5 (basic-fibonacci-sequence))
  (take 5 (stochastic-fibonacci-sequence))
  (take 5 (changing-rules-sequence))
  (take 5 (generational-sequence))
  (take 5 (stochastic-generational-sequence))
  (take 5 (dragon-sequence))
  )


(defrecord M1 [key color]
  ergo/Rewrite
  (module [_] key))

(defmethod clojure.core/print-method M1 [m writer]
  (.write writer (str "<" (ergo/module m) " " (:color m) ">")))

(defn parametric-system-example
  []
  (let [axiom [(->M1 :A :Red)]
        rules {:A [(->M1 :B :Light-Blue)
                   :-
                   (->M1 :A :Red)
                   :-
                   (->M1 :B :Dark-Blue)]
               :B [(->M1 :A :Dark-Red)
                   :+
                   (->M1 :B :Blue)
                   :+
                   (->M1 :A :Light-Red)]}]
    (ergo/parametric-rewriting-system axiom rules)))

(comment (take 5 (parametric-system-example)))


(defrecord M2 [key age]
  ergo/Rewrite (module [_] key))

(defn record-module-example
  []
  (let [axiom [(->M2 :A 0)]
        rules {:A (fn [g w i m]
                    [(->M2 :B 0)
                     :-
                     (->M2 (:key m) (inc (:age m)))
                     :-
                     (->M2 :B 0)])
               :B (fn [g w i m]
                    [(->M2 :A 0)
                     :+
                     (->M2 (:key m) (inc (:age m)))
                     :+
                     (->M2 :A 0)])}]
    (ergo/parametric-context-sensitive-rewriting-system axiom rules)))

(comment (take 5 (record-module-example)))


(deftype TM [key age]
  ergo/Rewrite
  (module [_] key)
  Object
  (toString [_] (str "<" key " " age ">")))

(defmethod clojure.core/print-method TM [x writer]
  (.write writer (str x)))

(defn type-module-example
  []
  (let [axiom [(->TM :A 0)]
        rules {:A (fn [g w i m]
                    [(->TM :B 0)
                     :-
                     (->TM (.key m) (inc (.-age m)))
                     :-
                     (->TM :B 0)])
               :B (fn [g w i m]
                    [(->TM :A 0)
                     :+
                     (->TM (.key m) (inc (.-age m)))
                     :+
                     (->TM :A 0)])}]
    (ergo/parametric-context-sensitive-rewriting-system axiom rules)))

(comment (take 5 (type-module-example)))


; -----------------------------------------------------------------------------
; Performance Benchmarking

(comment
  (cr/with-progress-reporting ;; 1.066 ms
    (cr/quick-bench (nth (dragon-sequence) 10) :verbose))

  (cr/with-progress-reporting ;; 910 ms
    (cr/quick-bench (nth (parametric-system-example) 10) :verbose))

  (cr/with-progress-reporting ;; 110 ms
    (cr/quick-bench (nth (record-module-example) 8) :verbose))

  (cr/with-progress-reporting ;; 123 ms
    (cr/quick-bench (nth (type-module-example) 8) :verbose))

  (cr/with-progress-reporting ;; 8.856 ms
    (cr/quick-bench (nth (basic-fibonacci-sequence) 20) :verbose))

  (cr/with-progress-reporting ;; 9.327 ms
    (cr/quick-bench (nth (stochastic-fibonacci-sequence) 20) :verbose))
  )
