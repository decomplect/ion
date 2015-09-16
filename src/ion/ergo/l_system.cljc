(ns ion.ergo.l-system
  "Support for Lindenmayer systems: deterministic, stochastic, context-free,
   context-sensitive, or parametric. Or combinations thereof.

   TL;DR How to produce recursive axiomatic transducible sequences (RATS)

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
   animation, or played as music. We call each word in this sequence a
   generation.

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
   context-sensitive calculation of successor module(s) (and, optionally,
   parameter data) to be returned by the function. To associate parameter data
   with a module, a successor must be defined as 2-element list where the first
   element is the module and the second element is an instance of any datatype,
   though it would usually be a map containing parameter keys and values.

   During the processing of a parametric system, any optional module parameter
   data is stored in a map associated by a key that is the index position of
   the module in the resulting word. Each generation of a system is therefore
   represented by a [word, data] pair, where data is a map containing the
   optional parametric data for the word sequence.")


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
; Shared Constants

(def axiom-key ::axiom)

(def jumpstart [axiom-key])


; -----------------------------------------------------------------------------
; Rewriting Processes

(defn context-free-process
  "Returns a lazy sequence of words from a context-free recursive axiomatic
   transducible process."
  [get-xf]
  (letfn [(process [w]
                   (lazy-seq
                     (when (seq w)
                       (let [word (into [] (get-xf) w)]
                         (cons word (process word))))))]
    (process jumpstart)))

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

(defn parametric-context-free-process
  "Returns a lazy sequence of [word data] pairs from a parametric context-free
   recursive axiomatic transducible process."
  [get-xf]
  (letfn [(process [w]
                   (lazy-seq
                     (when (seq w)
                       (let [temp (into [] (get-xf) w)
                             word (butlast temp)
                             data (peek temp)]
                         (cons [word data] (process word))))))]
    (process jumpstart)))

(defn parametric-contextual-process
  "Returns a lazy sequence of [word data] pairs from a parametric contextual
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
; Transformation Functions

(defn rewrite-module
  "Returns [m] or successors if m is found in the rules mapping."
  [rules m]
  (or (rules m) [m]))

(defn rewrite-module-with-context
  "Returns [i m [m]] or [i m [successors]] if m is found in the rules mapping."
  [rules [i m]]
  [i m (or (rules m) [m])])

(defn rules-for-gen
  "Return rules for generation, calling any rules written as functions."
  [axiom rules g]
  (if (= 0 g)
    {axiom-key axiom}
    (if (fn? rules) (rules g) rules)))

(defn rewrite
  "Returns a context-free rewriting transducer."
  [axiom rules g]
  (map (partial rewrite-module (rules-for-gen axiom rules g))))

(defn rewrite-with-context
  "Returns a contextual rewriting transducer."
  [axiom rules g]
  (map (partial rewrite-module-with-context (rules-for-gen axiom rules g))))

(defn call-without-context
  "Returns a function that, when called, will return successor or the result
   of calling successor."
  []
  (fn [successor] (if (fn? successor) (successor) successor)))

(defn call-with-context
  "Returns a function that, when called, will return successor or the result
   of calling successor with generation, word, index and module args."
  [g w]
  (fn [[i m successor]] (if (fn? successor) (successor g w i m) successor)))

(defn call-with-parametric-context
  "Returns a function that, when called, will return successor or the result
   of calling successor with generation, word, data, index and module args."
  [g w d]
  (fn [[i m successor]] (if (fn? successor) (successor g w d i m) successor)))

(defn split-module-parameters
  "Returns a stateful transducer that builds a map of module parameters, where
   each key is the index position of the module in the resulting word. Must
   appear in the transducing processs after any transducers that could filter
   out modules, so that the data index remains valid."
  []
  (fn [rf]
    (let [data (volatile! {})
          index (volatile! 0)
          split (fn [n successor]
                  (if (list? successor)
                    (do
                      (vswap! data assoc (+ @index n) (last successor))
                      (first successor))
                    successor))]
      (fn
        ([] (rf))
        ([result] (rf result [@data]))
        ([result input]
         (let [successor (into [] (map-indexed split input))]
           (vswap! index + (count successor))
           (rf result successor)))))))

(defn gen
  "Returns a function that, when called, will call f with an incremented
   generation value."
  [f]
  (let [generation (volatile! -1)]
    (fn []
      (vswap! generation inc)
      (f @generation))))

(defn gen-word
  "Returns a function that, when called, will call f with an incremented
   generation value and word."
  [f]
  (let [generation (volatile! -1)]
    (fn [word]
      (vswap! generation inc)
      (f @generation word))))

(defn gen-word-data
  "Returns a function that, when called, will call f with an incremented
   generation value, word and data."
  [f]
  (let [generation (volatile! -1)]
    (fn [word data]
      (vswap! generation inc)
      (f @generation word data))))


; -----------------------------------------------------------------------------
; Rewriting Systems (in order of increasing complexity/sophistication/power)

(defn basic-system
  "Returns a lazy sequence of words from a basic rewriting process."
  [axiom rules]
  (let [f-xf (gen (fn [g] (comp (rewrite axiom rules g)
                                cat)))]
    (context-free-process f-xf)))


; -----------------------------------------------------------------------------
; Functional System
;
; Extends the basic system by allowing a rewrite successor to (optionally) be
; a function that get called (without passing it any arguments).

(defn functional-system
  "Returns a lazy sequence of words from a rewriting process that allows a
   rewrite successor to (optionally) be a function that get called (without
   passing it any arguments)."
  [axiom rules]
  (let [f-xf (gen (fn [g] (comp (rewrite axiom rules g)
                                (map (call-without-context))
                                cat)))]
    (context-free-process f-xf)))


; -----------------------------------------------------------------------------
; Parametric System
;
; Allows a successor item to be a 2-element list containing a module and its
; parameters. Parameters would typically be stored in a map.
;
; Fundamentally alters the shape of the data coming out of the system from a
; sequence of words to a sequence of [word data] pairs.

(defn parametric-system
  "Returns a lazy sequence of [word data] pairs from a rewriting process,
   where word is a seq of modules and data is a map of module parameters keyed
   on the index value of the module's position in the word."
  [axiom rules]
  (let [f-xf (gen (fn [g] (comp (rewrite axiom rules g)
                                (split-module-parameters)
                                cat)))]
    (parametric-context-free-process f-xf)))


; -----------------------------------------------------------------------------
; Parametric-Functional System
;
; Combines the features of the functional and parametric systems. In addition,
; functions may return successors that include parametric data.

(defn parametric-functional-system
  "Returns a lazy sequence of [word data] pairs from a rewriting process,
   where word is a seq of modules and data is a map of module parameters keyed
   on the index value of the module's position in the word. Calls any successor
   functions without arguments, which may return successors that include
   parametric data."
  [axiom rules]
  (let [f-xf (gen (fn [g] (comp (rewrite axiom rules g)
                                (map (call-without-context))
                                (split-module-parameters)
                                cat)))]
    (parametric-context-free-process f-xf)))


; -----------------------------------------------------------------------------
; Contextual System

(defn contextual-system
  "Returns a lazy sequence of words from a contextual rewriting process."
  [axiom rules]
  (let [f-xf (gen-word (fn [g w]
                         (comp (rewrite-with-context axiom rules g)
                               (map (call-with-context g w))
                               cat)))]
    (contextual-process f-xf)))


; -----------------------------------------------------------------------------
; Parametric-Contextual System

(defn parametric-contextual-system
  "Returns a lazy sequence of [word data] pairs from a parametric-contextual
   rewriting process."
  [axiom rules]
  (let [f-xf (gen-word-data (fn [g w d]
                              (comp (rewrite-with-context axiom rules g)
                                    (map (call-with-parametric-context g w d))
                                    (split-module-parameters)
                                    cat)))]
    (parametric-contextual-process f-xf)))
