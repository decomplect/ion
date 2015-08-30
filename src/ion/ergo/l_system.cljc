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
   a Word is typically represented in an L-system by a `string`, although it
   doesn't need to be. And a Word is a sequence of somethings. For similar
   reasons we prefer to call those \"somethings\" Modules instead of the other
   commonly used term \"Letters\". (And so begins the inevitable confusion...)

   With that in mind, we describe an L-system as a parallel rewriting system.
   A rewriting system takes an intial input value made up of a collection of
   one or more symbols (we call that collection of symbols a \"Word\"), and
   recursively produces new \"Words\" by replacing each input symbol (called a
   Module) with itself or a successor symbol/module (or collection of modules)
   determined according to a set of production rules. The rewriting is treated
   as if the replacements all took place in parallel.

   L-systems operate on words, which are sequences of modules. When
   represented programatically, modules can be simple keywords or characters,
   or they can be compound structures containing additional parameters or local
   state.

   Beginning with an intial word, called an axiom, an L-system generates a
   developmental sequence of words by recusively applying a set of productions,
   or replacement rules. The resulting seqence of words can then be used as
   data for futher processing, such as to be rendered graphically or output as
   music.

   Because productions tend to replace one module with more than one module,
   words tend to grow in size. And because this growth is recursive, L-systems
   are good at modeling a variety of processes such as: fractal geometry, the
   growth of plants, morphogenesis, and more.")

(defn lookup
  "Returns key or the first item in key when key is a vector."
  [key]
  (if (sequential? key) (first key) key))

(defn rewrite
  "Returns [module] or a rewritten/successor module if key is found in the
   rules mapping. If replacement is a function it will be called."
  [rules generation word [index module]]
  (if-let [e (find rules (lookup module))]
    (let [replacement (val e)]
      (if (fn? replacement)
        (replacement generation word index module)
        replacement))
    [module]))

(defn process
  "Return a word resulting from rewriting each module in the original word."
  [rules generation word]
  (mapcat (partial rewrite rules generation word) (map vector (range) word)))

(defn generate
  "Returns a lazy sequence of words, starting with axiom, where each
   subsequent word is the result of a rewrite derivation of the preceding word."
  [rules axiom]
  (let [counter (atom 0)]
    (iterate #(process rules (swap! counter inc) %) (mapcat vector axiom))))

(defn words
  "Returns a lazy sequence of words for a grammar, beginning with axiom."
  [{:keys [axiom rules]}]
  (generate rules axiom))



#_(defn context
  "Returns a vector of left / right context values."
  [coll index]
  [(get coll (dec index)) (get coll (inc index))])

(defn age [v]
  (inc (:age (peek v))))

(def grammar
  {:foo-0
   {:axiom [:A]
    :rules {:A [:B :- :A :- :B]
            :B [:A :+ :B :+ :A]}}
   :foo-1
   {:axiom [:A]
    :rules {:A [:B :- :A :- :B]
            :B (fn [& _] (vec [:A :+ :B :+ :A]))}}
   :foo-2
   {:axiom [[:A {:age 0}]]
    :rules {:A #(vec [[:B {:age 0}] :- [:A {:age (age %4)}] :- [:B {:age 0}]])
            :B (fn [g c i v] (vec [[:A {:age 0}] :+ [:B {:age (age v)}] :+ [:A {:age 0}]]))}}
   :foo-3
   {:axiom [[:A {:age 0}]]
    :rules {:A [:B :- :A :- :B]
            :B (fn [g c i v] (vec [:A :+ :B :+ :A]))}}
   })

;(defn foo-words [] (words (:foo-2 grammar)))
