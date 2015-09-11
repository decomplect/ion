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
   optional parameters to be associated with each module. It also allows rules
   to be expressed using functions. If a rule's replacement value is a function
   it will be called and may be passed arguments that can be used to calculate
   the successor module(s) (and, optionally, data) to be returned by the
   function. To associate parameter data with a module, a successor module must
   be a 2-element list where the first element is the module and the second
   element is anything, though it would usually be a map containing parameter
   keys and values.

   During the processing of a parametric system, any optional module
   parameters are stored in a map associated with a key that is the index
   position of the module in the word. Each generation of a system can
   therefore be represented by a [word, data] pair, where data is a map
   containing the optional parametric data for the word sequence."

  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))


; -----------------------------------------------------------------------------
; Minimal Working System Examples

(comment
  "Minimal working examples of deterministic, context-free rewriting systems
   are illustrated here. The remainder of the code in this file is a result of
   the requirements for context-sensitive, parametric and stochastic systems.

   The breakthrough insight is to view an L-system as a specialized type of
   recursive axiomatic transducible (RAT) process."

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
; Shared Constants and Functions

(def axiom-key ::axiom)

(def jumpstart [axiom-key])

(defn f-identity [& _] identity)


; -----------------------------------------------------------------------------
; Rewriting Processes

(defn basic-process
  "Returns a lazy sequence of words from a recursive axiomatic transducible
   process."
  [get-xf]
  (letfn [(process [w]
                   (lazy-seq
                     (when (seq w)
                       (let [word (into [] (get-xf) w)]
                         (cons word (process word))))))]
    (process jumpstart)))

(defn parametric-process
  "Returns a lazy sequence of [word data] pairs from a parametric recursive
   axiomatic transducible process."
  [get-xf]
  (letfn [(process [w d]
                   (lazy-seq
                     (when (seq w)
                       (let [temp (into [] (get-xf) w)
                             word (butlast temp)
                             data (peek temp)]
                         (cons [word data] (process word data))))))]
    (process jumpstart {})))

(defn contextual-process
  "Returns a lazy sequence of words from a contextual recursive axiomatic
   transducible process."
  [get-xf]
  (letfn [(process [w]
                   (lazy-seq
                     (when (seq w)
                       (let [w-in (map-indexed vector w)
                             word (into [] (get-xf w) w-in)]
                         (cons word (process word))))))]
    (process jumpstart)))

(defn parametric-contextual-process
  "Returns a lazy sequence of [word data] pairs from a parametric-contextual
   recursive axiomatic transducible process."
  [get-xf]
  (letfn [(process [w d]
                   (lazy-seq
                     (when (seq w)
                       (let [w-in (map-indexed vector w)
                             temp (into [] (get-xf w d) w-in)
                             word (butlast temp)
                             data (peek temp)]
                         (cons [word data] (process word data))))))]
    (process jumpstart {})))


; -----------------------------------------------------------------------------
; Rewrite Functions

(defn rewrite-basic
  "Returns [m] or successors if m is found in the rules mapping."
  [rules m]
  (or (rules m) [m]))

(defn rewrite-indexed
  "Returns [i m [m]] or [i m [successors]] if m is found in the rules mapping."
  [rules [i m]]
  [i m (or (rules m) [m])])


; -----------------------------------------------------------------------------
; Basic Rewriting

(defn basic-rewriter
  "Returns a function that, when called, returns a basic rewriting transducer."
  ([axiom rules]
   (basic-rewriter axiom rules f-identity))
  ([axiom rules f-xf]
   (let [generation (volatile! -1)
         gen0-rules {axiom-key axiom}]
     (fn []
       (vswap! generation inc)
       (let [r (if (= 0 @generation)
                 gen0-rules
                 (if (fn? rules) (rules @generation) rules))]
         (comp (map (partial rewrite-basic r))
               (f-xf @generation)
               cat))))))


; -----------------------------------------------------------------------------
; Functional Rewriting
;
; Extends the basic system by allowing a rewrite successor to (optionally) be
; a function that get called (without passing it any arguments).

(defn call-without-arguments
  "Returns a function that, when called, will return successor or the result
   of calling successor."
  []
  (fn [successor] (if (fn? successor) (successor) successor)))

(defn functional-rewriter
  "Returns a function that, when called, returns a rewriting transducer."
  ([axiom rules]
   (let [f-xf (fn [_] (map (call-without-arguments)))]
     (basic-rewriter axiom rules f-xf)))
  ([axiom rules f-xf]
   (let [f-xf (fn [g] (comp (map (call-without-arguments)) (f-xf g)))]
     (basic-rewriter axiom rules f-xf))))


; -----------------------------------------------------------------------------
; Generational Rewriting
;
; Extends the functional system by supplying the generation as basic context.
;
; Provides the current generation as an argument to allow the customization of:
; a. the module(s) returned by (successor g) when successor is a function
; b. the function returned by (call-with-generation g)
; c. the transducer returned by (f-xf g)

(defn call-with-generation
  "Returns a function that, when called, will return successor or the result
   of calling successor with generation as an argument."
  [g]
  (fn [successor] (if (fn? successor) (successor g) successor)))

(defn generational-rewriter
  "Returns a function that, when called, returns a generational rewriting
   transducer."
  ([axiom rules]
   (let [f-xf (fn [g] (map (call-with-generation g)))]
     (basic-rewriter axiom rules f-xf)))
  ([axiom rules f-xf]
   (let [f-xf (fn [g] (comp (map (call-with-generation g)) (f-xf g)))]
     (basic-rewriter axiom rules f-xf))))


; -----------------------------------------------------------------------------
; Parametric Rewriting
;
; Allows a successor item to be a 2-element list containing a module and its
; parameters. Parameters would typically be stored in a map.
;
; Fundamentally alters the shape of the data coming out of the system from a
; sequence of words to a sequence of [word data] pairs.

(defn split-module-parameters
  "Returns a stateful transducer that builds a map of module parameters, where
   each key is the index position of the module in the resulting word. Must
   appear in the transducing processs after any transducers that could filter
   out modules, so that the data index remains valid."
  []
  (fn [rf]
    (let [data (volatile! {})
          index (volatile! 0)]
      (fn
        ([] (rf))
        ([result] (rf result [@data]))
        ([result input]
         (let [v (vec (for [[n, module] (map-indexed vector input)]  ; use mapv?
                        (if (list? module)
                          (let [[module param] module]  ; use reduce or loop/recur
                            (vswap! data assoc (+ @index n) param)
                            module)
                          module)))]
           (vswap! index + (count v))
           (rf result v)))))))

(defn parametric-rewriter
  "Returns a function that, when called, returns a rewriting transducer."
  ([axiom rules]
   (let [f-xf (fn [_] (split-module-parameters))]
     (basic-rewriter axiom rules f-xf)))
  ([axiom rules f-xf]
   (let [f-xf (fn [g] (comp (f-xf g)
                            (split-module-parameters)))]
     (basic-rewriter axiom rules f-xf))))


; -----------------------------------------------------------------------------
; Parametric-Functional Rewriting
;
; Combines the features of the functional and parametric systems. In addition,
; functions may return successors that include parametric data.

(defn parametric-functional-rewriter
  "Returns a function that, when called, returns a rewriting transducer."
  ([axiom rules]
   (let [f-xf (fn [_] (comp (map call-without-arguments)
                            (split-module-parameters)))]
     (basic-rewriter axiom rules f-xf)))
  ([axiom rules f-xf]
   (let [f-xf (fn [g] (comp (map call-without-arguments)
                            (f-xf g)
                            (split-module-parameters)))]
     (basic-rewriter axiom rules f-xf))))


; -----------------------------------------------------------------------------
; Parametric-Generational Rewriting
;
; Combines the features of the generational and parametric systems.

(defn parametric-generational-rewriter
  "Returns a function that, when called, returns a parametric-generational
   rewriting transducer."
  ([axiom rules]
   (let [f-xf (fn [g] (comp (map call-with-generation g)
                            (split-module-parameters)))]
     (basic-rewriter axiom rules f-xf)))
  ([axiom rules f-xf]
   (let [f-xf (fn [g] (comp (map call-with-generation g)
                            (f-xf g)
                            (split-module-parameters)))]
     (basic-rewriter axiom rules f-xf))))


; -----------------------------------------------------------------------------
; Contextual Rewriting

(defn call-with-context
  "Returns a function that, when called, will return successor or the result
   of calling successor with generation, word, index and module as arguments."
  [g w]
  (fn [[i m successor]] (if (fn? successor) (successor g w i m) successor)))

(defn contextual-rewriter
  "Returns a function that, when called, returns a contextual rewriting
   transducer."
  ([axiom rules]
   (contextual-rewriter axiom rules f-identity))
  ([axiom rules f-xf]
   (let [generation (volatile! -1)
         gen0-rules {axiom-key axiom}]
     (fn [w]
       (vswap! generation inc)
       (let [r (if (= 0 @generation)
                 gen0-rules
                 (if (fn? rules) (rules @generation) rules))]
         (comp (map (partial rewrite-indexed r))
               (map (call-with-context @generation w))
               (f-xf @generation w)
               cat))))))


; -----------------------------------------------------------------------------
; Parametric-Contextual Rewriting

(defn call-with-parametric-context
  "Returns a function that, when called, will return successor or the result
   of calling successor with generation, word, data, index and module as
   arguments."
  [g w d]
  (fn [[i m successor]] (if (fn? successor) (successor g w d i m) successor)))

(defn parametric-contextual-rewriter
  "Returns a function that, when called, returns a parametric-contextual
   rewriting transducer."
  ([axiom rules]
   (parametric-contextual-rewriter axiom rules f-identity))
  ([axiom rules f-xf]
   (let [generation (volatile! -1)
         gen0-rules {axiom-key axiom}]
     (fn [w d]
       (vswap! generation inc)
       (let [r (if (= 0 @generation)
                 gen0-rules
                 (if (fn? rules) (rules @generation) rules))]
         (comp (map (partial rewrite-indexed r))
               (map (call-with-parametric-context @generation w d))
               (f-xf @generation w d)
               (split-module-parameters)
               cat))))))


; -----------------------------------------------------------------------------
; Rewriting Systems (in order of increasing complexity/sophistication/power)

(defn basic-system
  "Returns a lazy sequence of words from a basic rewriting process."
  ([axiom rules]
   (basic-process (basic-rewriter axiom rules)))
  ([axiom rules f-xf]
   (basic-process (basic-rewriter axiom rules f-xf))))

(defn functional-system
  "Returns a lazy sequence of words from a rewriting process that allows a
   rewrite successor to (optionally) be a function that get called (without
   passing it any arguments)."
  ([axiom rules]
   (basic-process (functional-rewriter axiom rules)))
  ([axiom rules f-xf]
   (basic-process (functional-rewriter axiom rules f-xf))))

(defn generational-system
  "Returns a lazy sequence of words from a generational rewriting process.
   Calls any successor functions passing the generation as an argument."
  ([axiom rules]
   (basic-process (generational-rewriter axiom rules)))
  ([axiom rules f-xf]
   (basic-process (generational-rewriter axiom rules f-xf))))

(defn parametric-system
  "Returns a lazy sequence of [word data] pairs from a rewriting process,
   where word is a seq of modules and data is a map of module parameters keyed
   on the index value of the module's position in the word."
  ([axiom rules]
   (parametric-process (parametric-rewriter axiom rules)))
  ([axiom rules f-xf]
   (parametric-process (parametric-rewriter axiom rules f-xf))))

(defn parametric-functional-system
  "Returns a lazy sequence of [word data] pairs from a rewriting process,
   where word is a seq of modules and data is a map of module parameters keyed
   on the index value of the module's position in the word. Calls any successor
   functions without arguments, which may return successors that include
   parametric data."
  ([axiom rules]
   (parametric-process (parametric-functional-rewriter axiom rules)))
  ([axiom rules f-xf]
   (parametric-process (parametric-functional-rewriter axiom rules f-xf))))

(defn parametric-generational-system
  "Returns a lazy sequence of [word data] pairs from a parametric-generational
   rewriting process."
  ([axiom rules]
   (parametric-process (parametric-generational-rewriter axiom rules)))
  ([axiom rules f-xf]
   (parametric-process (parametric-generational-rewriter axiom rules f-xf))))

(defn contextual-system
  "Returns a lazy sequence of words from a contextual rewriting process."
  ([axiom rules]
   (contextual-process (contextual-rewriter axiom rules)))
  ([axiom rules f-xf]
   (contextual-process (contextual-rewriter axiom rules f-xf))))

(defn parametric-contextual-system
  "Returns a lazy sequence of [word data] pairs from a parametric-contextual
   rewriting process."
  ([axiom rules]
   (parametric-contextual-process
     (parametric-contextual-rewriter axiom rules)))
  ([axiom rules f-xf]
   (parametric-contextual-process
     (parametric-contextual-rewriter axiom rules f-xf))))


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
  (apply basic-system (grammar :A003849)))

(deftest fibonacci-sequence-basic-test
  (is (= 144 (-> (basic-fibonacci-sequence) (nth 10) count))))


(defn stochastic-fibonacci-sequence
  "Returns a lazy sequence of vectors of Fibonacci integers starting randomly
   with 0 or 1."
  []
  (let [axiom #(vec [(rand-int 2)])
        rules {0 [0 1]
               1 [0]}]
    (functional-system axiom rules)))


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
    (basic-system axiom rules)))


(defn generational-sequence
  "Returns a lazy sequence of integers."
  []
  (let [axiom [0]
        rules {0 (fn [g] [0 g 1])
               1 []
               2 [0]
               3 [1 2 3 4]
               4 []}]
    (generational-system axiom rules)))


(defn stochastic-generational-sequence
  "Returns a lazy sequence of semi-random integers."
  []
  (let [axiom [0]
        rules {0 (fn [g] [0 (rand-int (+ g 5)) 1])
               1 [0]}]
    (generational-system axiom rules)))


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
    (parametric-system axiom rules)))

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
    (parametric-contextual-system axiom rules)))

(comment (take 5 (parametric-contextual-system-example)))


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
   })
