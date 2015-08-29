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
  [rules generation coll [index v]]
  (if-let [e (find rules (lookup v))]
    (let [replacement (val e)]
      (if (fn? replacement)
        (replacement generation coll index v)
        replacement))
    [v]))

(defn process
  "Return a coll resulting from rewriting each item in the original coll."
  [rules generation coll]
  (mapcat (partial rewrite rules generation coll) (map vector (range) coll)))

(defn generate
  "Returns a lazy sequence of colls, starting with axiom, where each
   subsequent coll is the result of a rewrite derivation of the preceding coll."
  [rules axiom]
  (let [counter (atom 0)]
    (iterate #(process rules (swap! counter inc) %) (mapcat vector axiom))))

(defn modules
  "Returns a lazy sequence of modules for a grammar."
  [{:keys [axiom rules]}]
  (generate rules axiom))

(defn context
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

;(defn foo-modules [] (modules (:foo-2 grammar)))
