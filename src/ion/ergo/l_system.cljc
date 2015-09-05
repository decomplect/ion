(ns ion.ergo.l-system
  "Support for Lindenmayer systems: deterministic, stochastic, context-free,
   context-sensitive, or parametric. Or combinations thereof.

   A somewhat confusing variety of terms are used to describe L-systems and
   their component parts, and this source code file is likely no exception. In
   order to be understood in the context of existing semiotics we have adhered
   as closely as possible to the most commonly used terminology. In particular,
   however, we avoid using the term \"String\" so as to avoid confusion with
   the datatype of the same name. Instead we refer to that part of the system
   as a \"Word\", although it should not be confused with the everyday notion
   of a word in a spoken language. And while we avoid calling a Word a String,
   a Word is often represented in an L-system by a `string`, although it
   doesn't need to be (and isn't here). And a Word is a sequence of somethings.
   For similar reasons we prefer to call those \"somethings\" Modules instead
   of the other commonly used term \"Letters\". (And so begins the inevitable
   confusion...)

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
   animation, or played as music. We call each word in this sequence a
   generation.

   Because productions typically replace each module with more than one
   module, words tend to grow in size with each successive generation. And
   because this growth is recursive, L-systems are useful for modeling a
   variety of mathematical and natural processes such as: fractal geometry, the
   growth and branching of plants, morphogenesis, crystallography, and more.

   To fully support more advanced processing, this implementation allows
   optional state data to be associated with each module. It also allows rules
   to be expressed using functions. If a rule's replacement value is a function
   it will be called and passed a set of arguments that can be used to
   calculate the successor module(s) (and, optionally, data) to be returned by
   the function. To associate state data with a module, a successor module must
   be a 2-element list where the first element is the module and the second
   element is anything, though it would usually be a map containing property
   values.

   During the processing of productions, any optional module state data is
   stored in a map using a key that is the index position of the module in the
   word. Each generation of a system is therefore represented by a [word,
   state] pair, where state is a map containing the optional data for the word
   sequence. If a grammar does not make use of state data, the state in each
   [word, state] pair will simply be an empty map."

  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))


; -----------------------------------------------------------------------------
; Minimal Working System Examples

(comment
  "Minimal working examples of deterministic, context-free rewriting systems
   are illustrated here. The remainder of the code in this file is a result of
   the requirements for context-sensitive, parametric and stochastic systems.
   The breakthrough is to view these as recursive axiomatic transducible
   processes."

  (def axiom [0])

  (def rules {0 [0 1]
              1 [0]})

  (defn minimal-working-system
    [rules axiom]
    (iterate #(apply concat (replace rules %)) (seq axiom)))

  (take 20 (minimal-working-system rules axiom))

  (count (nth (minimal-working-system rules axiom) 35)) ; 24157817

  (defn minimal-working-system-b
    [rules axiom]
    (iterate (fn [word] (mapcat #(or (rules %) [%]) word)) (seq axiom)))

  )


; -----------------------------------------------------------------------------
; Transducers

(defn replace-xform
  "Returns a transducer that will apply the replacement rules to a word."
  [rules]
  (map #(or (rules %) [%])))

(deftest replace-xform-test
  (let [input  [0 1 2 3]
        output [[0 1] [0 2] [2] [3]]
        rules  {0 [0 1] 1 [0 2]}]
    (is (= output (transduce (replace-xform rules) conj input)))))

(comment
  "One failing and one passing test to explain why we needed replace-xform."
  (deftest replace-fails-with-cat
  ; Don't know how to create ISeq from: java.lang.Long
    (let [input [0 1 2 3]
          output [0 1 0 2 2 3]
          rules {0 [0 1] 1 [0 2]}]
      (is (= output (transduce (comp (replace rules) cat) conj input)))))
  (deftest replace-xform-works-with-cat
    (let [input [0 1 2 3]
          output [0 1 0 2 2 3]
          rules {0 [0 1] 1 [0 2]}]
      (is (= output (transduce (comp (replace-xform rules) cat) conj input)))))
  )

(defn call-with-arguments-xform
  "Returns a transducer that will call any function with arguments."
  [& more]
  (map #(if (fn? %) (apply % more) %)))

(defn call-without-arguments-xform
  "Returns a transducer that will call any function without passing arguments."
  []
  (map #(if (fn? %) (%) %)))


; -----------------------------------------------------------------------------
; Lindenmayer Systems

(defn system
  "Returns a recursive axiomatic transducible process."
  ([axiom rules]
   (system axiom rules (fn [] identity)))
  ([axiom rules f]
   (system axiom rules f conj))
  ([axiom rules f rf]
   (let [key :axiom
         rules (merge {key axiom} rules)
         get-xf #(comp (replace-xform rules) (f) cat)
         process (fn [word] (transduce (get-xf) rf word))
         init (process [key])]
     (iterate process init))))

(defn generational-system
  "Returns a generational recursive axiomatic transducible process."
  ([axiom rules]
   (generational-system axiom rules (fn [_] identity)))
  ([axiom rules f]
   (generational-system axiom rules f conj))
  ([axiom rules f rf]
   (let [generation (atom -1)
         key :axiom
         rules (merge {key axiom} rules)
         get-xf (fn [g] (comp (replace-xform rules) (f g) cat))
         process (fn [[_ word]]
                   (swap! generation inc)
                   [@generation (transduce (get-xf @generation) rf word)])
         init (process [@generation [key]])]
     (iterate process init))))


; -----------------------------------------------------------------------------
; Example Systems

(defn fibonacci-sequence-basic
  "Returns a lazy sequence of vectors of Fibonacci integers - OEIS A003849."
  []
  (let [axiom [0]
        rules {0 [0 1]
               1 [0]}]
    (system axiom rules)))

(deftest fibonacci-sequence-basic-test
  (is (= 144 (-> (fibonacci-sequence-basic) (nth 10) count))))


(defn fibonacci-sequence-generational
  "Returns a lazy sequence of [generation [Fibonacci integers]]."
  []
  (let [axiom [0]
        rules {0 [0 1]
               1 [0]}]
    (generational-system axiom rules)))

(deftest fibonacci-sequence-generational-test
  (is (= 144 (-> (fibonacci-sequence-generational) (nth 10) peek count))))


(defn fibonacci-sequence-stochastic
  "Returns a lazy sequence of vectors of Fibonacci integers starting randomly
   with 0 or 1."
  []
  (let [axiom #(vec [(rand-int 2)])
        rules {0 [0 1]
               1 [0]}]
    (system axiom rules call-without-arguments-xform)))


(defn generational-stochastic-sequence
  "Returns a lazy sequence of [generation [semi-random-integers]] pairs."
  []
  (let [axiom [0]
        rules {0 (fn [g] [0 (rand-int (+ g 5)) 1])
               1 [0]}
        f (fn [g] (call-with-arguments-xform g))]
    (generational-system axiom rules f)))


(comment
  (take 5 (fibonacci-sequence-basic))
  (take 5 (fibonacci-sequence-generational))
  (take 5 (fibonacci-sequence-stochastic))
  (take 5 (generational-stochastic-sequence))
  )


; -----------------------------------------------------------------------------
; Old Code

(defn split-successor
  "Returns a sequence of modules, updating new-state with any successor data."
  [successor new-state index]
  (doall
    (for [[n, module] (map vector (range) successor)]
      (if (list? module)
        (let [[module data] module]
          (swap! new-state assoc (+ index n) data)
          module)
        module))))

(defn rewrite
  "Returns [module] or a successor module if module is found in the rules
   mapping. If successor is a function it will be called."
  [rules generation word state new-state new-index [index module]]
  (if-let [e (find rules module)]
    (let [successor (val e)
          successor (if (fn? successor)
                      (successor generation word state index module)
                      successor)
          successor (split-successor successor new-state @new-index)]
      (swap! new-index + (count successor))
      successor)
    (do
      (swap! new-index inc)
      [module])))

(defn process
  "Returns new [word state] pair resulting from rewriting each module in the
   original word and updating the properties in state."
  [rules generation word state]
  (let [new-state (atom {})
        new-index (atom 0)
        rewriter (partial rewrite rules generation word state new-state new-index)
        new-word (doall (mapcat rewriter (map vector (range) word)))]
    [new-word @new-state]))

(defn generate
  "Returns a lazy sequence of [word state] pairs, where each subsequent word is
   the result of a rewrite derivation of the preceding word."
  [rules axiom]
  (let [counter (atom 0)
        genproc (fn [[word state]] (process rules (swap! counter inc) word state))
        initial (process {:init axiom} @counter [:init] {})]
    (iterate genproc initial)))

(defn gen
  "Returns a lazy sequence of [word state] pairs for a grammar."
  [{:keys [axiom rules]}]
  (generate rules axiom))


#_(defn context
  "Returns a vector of left / right context values."
  [word index]
  [(get word (dec index)) (get word (inc index))])


(defn age [state index]
  (inc (:age (get state index))))


; When a grammar produces an integer sequence it is named after its identifier
; from "The On-Line Encyclopedia of Integer Sequences" https://oeis.org/

(def grammar
  {:A003849 ; Fibonacci sequence
   {:axiom [0]
    :rules {0 [0 1]
            1 [0]}}
   ;:A005614 ; Fibonacci sequence
   ;{:axiom [1]
   ; :rules {0 [1]
   ;         1 [1 0]}}
   :A010060 ; Thue-Morse sequence
   {:axiom [0]
    :rules {0 [0 1]
            1 [1 0]}}
   :A014577 ; Dragon-curve sequence
   {:axiom [:L]
    :rules {:L [:L :1 :R]
            :R [:L :0 :R]}}
   :A026465 ; Length of n-th run of identical symbols in the Thue-Morse sequence A010060
   {:axiom [1]
    :rules {1 [1 2 1]
            2 [1 2 2 2 1]}}
   :A029883 ; First differences of Thue-Morse sequence A001285
   {:axiom [1]
    :rules {1 [1 0 -1]
            0 [1 -1]
            -1 [0]}}
   :A036577 ; Ternary Thue-Morse sequence
   {:axiom [2]
    :rules {0 [1]
            1 [2 0]
            2 [2 1 0]}}
   :A166253
   {:axiom [1]
    :rules {0 [0 1 1 1 0]
            1 [1 0 0 0 1]}}
   :template
   {:axiom [0]
    :rules {0 []
            1 []}}
   :Rudin-Shapiro-sequence
   {:axiom [:AA]
    :rules {:AA [:AA :AB]
            :AB [:AA :BA]
            :BA [:BB :AB]
            :BB [:BB :BA]}}
   :foo-0
   {:axiom [:A]
    :rules {:A [:B :- :A :- :B]
            :B [:A :+ :B :+ :A]}}
   :foo-1
   {:axiom [:A]
    :rules {:A [:B :- :A :- :B]
            :B (fn [& _] [:A :+ :B :+ :A])}}
   :foo-2
   {:axiom ['(:A {:color :Red})]
    :rules {:A ['(:B {:color :Light-Blue}) :- '(:A {:color :Red}) :- '(:B {:color :Dark-Blue})]
            :B ['(:A {:color :Dark-Red}) :+ (list :B {:color :Blue}) :+ '(:A {:color :Light-Red})]}}
   :foo-3
   {:axiom ['(:A {:age 0})]
    :rules {:A #(vec ['(:B {:age 0}) :- (list %5 {:age (age %3 %4)}) :- '(:B {:age 0})])
            :B (fn [g w s i m] ['(:A {:age 0}) :+ (list m {:age (age s i)}) :+ '(:A {:age 0})])}}
   :foo-4
   {:axiom ['(:A {:age 0})]
    :rules {:A [:B :- :A :- :B]
            :B (fn [g w s i m] [:A :+ :B :+ :A])}}
   })
