(ns ion.ergo.l-system
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
   defrecord that satifsfies the Module protocol.")


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
; RAT Process

(defn produce
  "Returns a lazy sequence of words from a recursive, axiomatic, transducible
   process."
  [get-xf]
  (letfn [(process [w]
                   (lazy-seq
                     (when (seq w)
                       (let [v (vec w)
                             word (into [] (get-xf v) v)]
                         (cons word (process word))))))]
    (process jumpstart)))


; -----------------------------------------------------------------------------
; Protocols

(defprotocol Module (module [m]))


; -----------------------------------------------------------------------------
; Transformation Functions

(defn modulate
  "Returns the module or the module supplied by an object implementing the
   Module protocol."
  [m]
  (if (satisfies? Module m) (module m) m))

(defn rewrite
  "Returns a successor, which must be a vector or a function. If no match is
   found in the rules mapping, the original module is return within a vector."
  [rules m]
  (or (rules m) [m]))

(defn call
  "Returns a function that will return the successor vector, or the result of
   calling the successor function, which must return a vector of modules."
  ([]
   (fn context-free-call [successor]
     (if (fn? successor)
       (successor)
       successor)))
  ([g w]
   (let [index (volatile! -1)]
     (fn context-sensitive-call [successor]
       (vswap! index inc)
       (if (fn? successor)
         (successor g w @index (get w @index))
         successor)))))


; -----------------------------------------------------------------------------
; Transducers

(defn modulating
  "Returns a module-extracting transducer."
  []
  (map modulate))

(defn get-rules
  "Returns the rules for a generation, calling any rules written as functions."
  [axiom rules g]
  (if (= 0 g)
    {axiom-key axiom}
    (if (fn? rules) (rules g) rules)))

(defn rewriting
  "Returns a rewriting transducer."
  [axiom rules g]
  (map (partial rewrite (get-rules axiom rules g))))

(defn calling
  "Returns a function-calling transducer."
  ([]
   (map (call)))
  ([g w]
   (map (call g w))))


; -----------------------------------------------------------------------------
; Generation and Word Context

(defn gen
  "Returns a function that, when called, will call f with an incremented
   generation value and word."
  [f]
  (let [generation (volatile! -1)]
    (fn
      [word]
      (vswap! generation inc)
      (f @generation word))))


; -----------------------------------------------------------------------------
; Rewriting Systems

(defn basic-system
  "Returns a lazy sequence of words from a context-free rewriting process."
  [axiom rules]
  (produce (gen (fn [g _]
                  (comp (rewriting axiom rules g)
                        cat)))))

(defn functional-system
  "Returns a lazy sequence of words from a context-free rewriting process.
   Allows a rewrite successor to (optionally) be a function that will get
   called without passing it any arguments. The returned value may be
   deterministic or stochastic."
  [axiom rules]
  (produce (gen (fn [g _]
                  (comp (rewriting axiom rules g)
                        (calling)
                        cat)))))

(defn context-sensitive-system
  "Returns a lazy sequence of words from a context-sensitive rewriting
   process. Allows a rewrite successor to (optionally) be a function that will
   get called with the current context as arguments."
  [axiom rules]
  (produce (gen (fn [g w]
                  (comp (rewriting axiom rules g)
                        (calling g w)
                        cat)))))

(defn parametric-system
  "Returns a lazy sequence of words from a context-free rewriting process."
  [axiom rules]
  (produce (gen (fn [g _]
                  (comp (modulating)
                        (rewriting axiom rules g)
                        cat)))))

(defn parametric-functional-system
  "Returns a lazy sequence of words from a context-free rewriting process.
   Allows a rewrite successor to (optionally) be a function that will get
   called without passing it any arguments. The returned value may be
   deterministic or stochastic."
  [axiom rules]
  (produce (gen (fn [g _]
                  (comp (modulating)
                        (rewriting axiom rules g)
                        (calling)
                        cat)))))

(defn parametric-context-sensitive-system
  "Returns a lazy sequence of words from a context-sensitive rewriting
   process. Allows a rewrite successor to (optionally) be a function that will
   get called with the current context as arguments."
  [axiom rules]
  (produce (gen (fn [g w]
                  (comp (modulating)
                        (rewriting axiom rules g)
                        (calling g w)
                        cat)))))
