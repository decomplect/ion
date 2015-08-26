(ns ion.ergo.l-system
  "A D0L system (deterministic context-free Lindenmayer system)")

(defn rewrite
  "Returns a new coll with the items in coll replaced according to the rules
   mapping. Items without a match in the rules are retained as-is."
  [rules coll]
  (mapcat #(or (rules %) [%]) coll))

(defn generate
  "Returns a lazy sequence of colls, starting with axiom, where each
   subsequent coll is the result of the replacement rules applied to the
   preceding coll."
  [rules axiom]
  (iterate (partial rewrite rules) (mapcat vector axiom)))

(defn words
  "Returns a lazy sequence of words for a grammar."
  [{:keys [rules axiom]}]
  (generate rules axiom))

(def grammar {:foo
              {:axiom [:A]
               :rules {:A [:B :- :A :- :B]
                       :B [:A :+ :B :+ :A]}}})

(def foo-words (words (:foo grammar)))
