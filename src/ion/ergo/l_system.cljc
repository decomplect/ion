(ns ion.ergo.l-system
  "Support for Lindenmayer systems: deterministic, stochastic, context-free,
   context-sensitive, or parametric. Or combinations thereof.")

(defn lookup
  "Returns key or the first item in key when key is a vector."
  [key]
  (if (sequential? key) (first key) key))

(defn rewrite
  "Returns [v] or a replacement value if key is found in the rules mapping.
   If replacement is a function it will be called."
  [rules context? coll n v]
  (if-let [e (find rules (lookup v))]
    (let [replacement (val e)]
      (if (fn? replacement)
        (if context?
          (replacement v (get coll (dec n)) (get coll (inc n)))
          (replacement v))
        replacement))
    [v]))

(defn evaluate
  [f context? coll]
  (if context?
    (mapcat #(apply (partial f coll) %) (map vector (range) coll))
    (mapcat (partial f nil nil) coll)))

(defn generate
  "Returns a lazy sequence of colls, starting with axiom, where each
   subsequent coll is the result of mapcat f applied to the preceding coll."
  [f axiom context?]
  (iterate (partial evaluate f context?) (mapcat vector axiom)))

(defn modules
  "Returns a lazy sequence of modules for a grammar."
  [{:keys [axiom context? rules]}]
  (generate (partial rewrite rules context?) axiom context?))

(defn age [v]
  (inc (:age (peek v))))

(def grammar
  {:foo-1
   {:axiom [:A]
    :context? false
    :rules {:A [:B :- :A :- :B]
            :B #(vec [:A :+ :B :+ :A])
            :C (fn [v] ())}}
   :foo-2
   {:axiom [[:A {:age 0}]]
    :context? false
    :rules {:A #(vec [[:B {:age 0}] :- [:A {:age (age %)}] :- [:B {:age 0}]])
            :B (fn [v] (vec [[:A {:age 0}] :+ [:B {:age (age v)}] :+ [:A {:age 0}]]))}}
   :foo-3
   {:axiom [[:A {:age 0}]]
    :context? true
    :rules {:A [:B :- :A :- :B]
            :B (fn [v prev post] (vec [:A :+ :B :+ :A]))}}
   })

(def foo-modules (modules (:foo-2 grammar)))
