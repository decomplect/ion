(ns ion.ergo.lindenmayer-systems
  "Support for Lindenmayer systems: deterministic, stochastic, context-free,
   context-sensitive, or parametric. Or combinations thereof.

   TL;DR: How to produce recursive axiomatic transducible sequences (RATS)

   A somewhat confusing variety of terms are used to describe L-systems and
   their component parts, and this source code file is likely no exception. In
   order to be understood in the context of existing semiotics we have adhered
   as closely as possible to the most commonly used domain terminology in favor
   of Clojure terminology. In particular, however, we avoid using the term
   \"String\" so as to avoid confusion with the datatype of the same name.
   Instead we refer to that part of the system as a \"Word\", although it
   should not be confused with the everyday notion of a word in a spoken
   language. And while we avoid calling a Word a String, a Word is often
   represented in an L-system by a `string`, although it doesn't need to be
   (and isn't here). And a Word is a sequence of somethings. For similar
   reasons we prefer to call those \"somethings\" Modules instead of the other
   commonly used term \"Letters\". (And so begins the inevitable confusion...)

   With that in mind, we describe an L-system as a parallel rewriting system.
   A rewriting system takes an intial input value made up of a sequence of one
   or more symbols (we call that sequence of symbols a \"Word\"), and
   recursively produces new \"Words\" by replacing each input symbol (called a
   Module) with itself or a successor symbol/module (or sequence of modules)
   determined according to a set of production rules. The rewriting is treated
   as if the replacements all took place in parallel for each generation.

   L-systems operate on words, which are sequences of modules. When
   represented programatically, modules are typically simple integers,
   character literals, strings or keywords. This implementation allows a module
   to be any value that is a valid key in a map. So a module can also be a
   compound structure, such as a vector containing a pair of integers.

   Beginning with an initial word, called an axiom, an L-system generates a
   developmental sequence of words by recusively applying a set of productions,
   or replacement rules. The resulting seqence of words can then be used as
   data for futher processing, such as to be rendered as a graphic or
   animation, or played as music.

   Because productions typically replace each module with more than one
   module, words tend to grow in size with each successive generation. And
   because this growth is recursive, L-systems are useful for modeling a
   variety of mathematical and natural processes such as: fractal geometry, the
   growth and branching of cells and plants, morphogenesis, crystallography,
   architecture, caves, generated game content, and more.

   To fully support more advanced processing, this implementation allows
   optional parameters to be associated with a module. It also allows rules to
   be expressed using functions. If a rule's replacement value is a function it
   will be called and can also be passed arguments for use in the
   context-sensitive calculation of successor modules. To associate parameter
   data with a module, a successor module must be defined as a deftype or
   defrecord that satifsfies the Module protocol."

  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [criterium.core :as cr]
                     [ion.ergo.core :as ergo]))


; -----------------------------------------------------------------------------
; Minimal Working System Examples

(comment
  "Minimal working examples of deterministic, context-free rewriting systems
   are illustrated here. The remainder of the code in this file is a result of
   the requirements for context-sensitive, parametric and stochastic systems.

   The breakthrough insight is to view L-systems as specialized types of
   recursive axiomatic transducible sequences (RATS)."

  (def axiom [0])

  (def rules {0 [0 1]
              1 [0]})

  (defn minimal-working-system-a
    [rules axiom]
    (iterate #(apply concat (replace rules %)) (seq axiom)))

  (take 20 (minimal-working-system-a rules axiom))

  (count (nth (minimal-working-system-a rules axiom) 35)) ; 24157817

  (defn minimal-working-system-b
    [rules axiom]
    (iterate (fn [word] (mapcat #(or (rules %) [%]) word)) (seq axiom)))

  )


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
  ergo/IRewritable
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
  ergo/IRewritable (module [_] key))

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
  ergo/IRewritable
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
