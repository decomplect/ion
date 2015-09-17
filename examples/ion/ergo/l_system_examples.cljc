(ns ion.ergo.l-system-examples
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [criterium.core :as cr]
                     [ion.ergo.l-system :as ls :refer [->MP]]))

; -----------------------------------------------------------------------------
; Helper Functions

(declare grammarpedia)

(defn grammar
  [key]
  (let [gramm (key grammarpedia)]
    [(:axiom gramm) (:rules gramm)]))

(defn age
  "Returns the age parameter value for the module at index."
  [data index]
  (-> data (get index) :age))

(defn neighbors
  "Returns a vector of left / right neighbor values."
  [word index]
  [(get word (dec index)) (get word (inc index))])


; -----------------------------------------------------------------------------
; Example Systems

(defn basic-fibonacci-sequence
  "Returns a lazy sequence of vectors of Fibonacci integers - OEIS A003849."
  []
  (apply ls/basic-system (grammar :A003849)))

(deftest fibonacci-sequence-basic-test
  (is (= 144 (-> (basic-fibonacci-sequence) (nth 10) count))))


(defn stochastic-fibonacci-sequence
  "Returns a lazy sequence of vectors of Fibonacci integers starting randomly
   with 0 or 1."
  []
  (let [axiom #(vec [(rand-int 2)])
        rules {0 [0 1]
               1 [0]}]
    (ls/functional-system axiom rules)))


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
    (ls/basic-system axiom rules)))


(defn stochastic-generational-sequence
  "Returns a lazy sequence of semi-random integers."
  []
  (let [axiom [0]
        rules (fn [g]
                {0 [0 (rand-int (+ g 5)) 1]
                 1 [0]})]
    (ls/basic-system axiom rules)))


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
    (ls/basic-system axiom rules)))


(comment
  (take 5 (basic-fibonacci-sequence))
  (take 5 (stochastic-fibonacci-sequence))
  (take 5 (changing-rules-sequence))
  (take 5 (generational-sequence))
  (take 5 (stochastic-generational-sequence))
  )


(defn parametric-system-example
  []
  (let [axiom ['(:A {:color :Red})]
        rules {:A ['(:B {:color :Light-Blue})
                   :-
                   '(:A {:color :Red})
                   :-
                   '(:B {:color :Dark-Blue})]
               :B ['(:A {:color :Dark-Red})
                   :+
                   (list :B {:color :Blue})
                   :+
                   '(:A {:color :Light-Red})]}]
    (ls/parametric-system axiom rules)))

(comment (take 5 (parametric-system-example)))


(defn parametric-contextual-system-example
  []
  (let [axiom ['(:A {:age 0})]
        rules {:A (fn [g w d i m]
                    ['(:B {:age 0})
                     :-
                     (list m {:age (inc (age d i))})
                     :-
                     '(:B {:age 0})])
               :B (fn [g w d i m]
                    ['(:A {:age 0})
                     :+
                     (list m {:age (inc (age d i))})
                     :+
                     '(:A {:age 0})])}]
    (ls/parametric-contextual-system axiom rules)))

(comment (take 5 (parametric-contextual-system-example)))


(defn parametric-module-example
  []
  (let [axiom [(->MP :A {:age 0})]
        rules {:A (fn [g w d i m]
                    [(->MP :B {:age 0})
                     :-
                     (->MP m {:age (inc (age d i))})
                     :-
                     (->MP :B {:age 0})])
               :B (fn [g w d i m]
                    [(->MP :A {:age 0})
                     :+
                     (->MP m {:age (inc (age d i))})
                     :+
                     (->MP :A {:age 0})])}]
    (ls/parametric-contextual-system axiom rules)))

(comment (take 5 (parametric-module-example)))


; -----------------------------------------------------------------------------
; Performance Benchmarking

(comment
  (cr/with-progress-reporting
    (cr/quick-bench (nth (basic-fibonacci-sequence) 10) :verbose)))

(comment
  (cr/with-progress-reporting
    (cr/quick-bench (nth (parametric-system-example) 10) :verbose)))

(comment
  (cr/with-progress-reporting
    (cr/quick-bench (nth (parametric-contextual-system-example) 10) :verbose)))

(comment
  (cr/with-progress-reporting
    (cr/quick-bench (nth (parametric-module-example) 10) :verbose)))


; -----------------------------------------------------------------------------
; Grammarpedia

; When a grammar produces an integer sequence, it is named after its identifier
; from "The On-Line Encyclopedia of Integer Sequences" https://oeis.org/

(def grammarpedia
  {:A003849
   {:descr "Fibonacci sequence, beginning with zero"
    :axiom [0]
    :rules {0 [0 1]
            1 [0]}}
   :A005614
   {:descr "Fibonacci sequence, beginning with one"
    :axiom [1]
    :rules {0 [1]
            1 [1 0]}}
   :A010060
   {:descr "Thue-Morse sequence"
    :axiom [0]
    :rules {0 [0 1]
            1 [1 0]}}
   :A014577
   {:descr "Dragon-curve sequence"
    :axiom [:L]
    :rules {:L [:L :1 :R]
            :R [:L :0 :R]}}
   :A026465
   {:descr "Length of n-th run of identical symbols in the Thue-Morse sequence A010060"
    :axiom [1]
    :rules {1 [1 2 1]
            2 [1 2 2 2 1]}}
   :A029883
   {:descr "First differences of Thue-Morse sequence A001285"
    :axiom [1]
    :rules {1 [1 0 -1]
            0 [1 -1]
            -1 [0]}}
   :A036577
   {:descr "Ternary Thue-Morse sequence"
    :axiom [2]
    :rules {0 [1]
            1 [2 0]
            2 [2 1 0]}}
   :A166253
   {:descr "A166253"
    :axiom [1]
    :rules {0 [0 1 1 1 0]
            1 [1 0 0 0 1]}}
   :Rudin-Shapiro-sequence
   {:descr "Rudin-Shapiro sequence"
    :axiom [:AA]
    :rules {:AA [:AA :AB]
            :AB [:AA :BA]
            :BA [:BB :AB]
            :BB [:BB :BA]}}
   :binary
   {:descr "Binary sequence"
    :axiom [0]
    :rules {0 [0 1 0]}}
   })
