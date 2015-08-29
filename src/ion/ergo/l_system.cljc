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
  [rules context? generation coll n v]
  (if-let [e (find rules (lookup v))]
    (let [replacement (val e)]
      (if (fn? replacement)
        (if context?
          (replacement generation v (get coll (dec n)) (get coll (inc n)))
          (replacement generation v))
        replacement))
    [v]))

(defn derive
  "Return the successor coll resulting from rewriting each item in the
   predecessor coll."
  [f context? generation coll]
  (if context?
    (mapcat #(apply (partial f generation coll) %) (map vector (range) coll))
    (mapcat (partial f generation nil nil) coll)))

(defn generate
  "Returns a lazy sequence of colls, starting with axiom, where each
   subsequent coll is the result of mapcat f applied to the preceding coll."
  [f axiom context?]
  (let [counter (atom 0)
        step (partial derive f context?)]
    (iterate #(step (swap! counter inc) %) (mapcat vector axiom))))

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
            :B #(vec [:A :+ :B :+ :A])}}
   :foo-2
   {:axiom [[:A {:age 0}]]
    :context? false
    :rules {:A #(vec [[:B {:age 0}] :- [:A {:age (age %2)}] :- [:B {:age 0}]])
            :B (fn [g v] (vec [[:A {:age 0}] :+ [:B {:age (age v)}] :+ [:A {:age 0}]]))}}
   :foo-3
   {:axiom [[:A {:age 0}]]
    :context? true
    :rules {:A [:B :- :A :- :B]
            :B (fn [g v l r] (vec [:A :+ :B :+ :A]))}}
   })

;(defn foo-modules [] (modules (:foo-2 grammar)))
