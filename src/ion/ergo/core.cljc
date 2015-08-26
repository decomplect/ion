(ns ion.ergo.core)

(def ^:const PI Math/PI)

(def ^:const THREE-HALVES-PI (* PI 1.5))
(def ^:const TWO-PI (* PI 2.0))

(def ^:const HALF-PI (/ PI 2.0))
(def ^:const THIRD-PI (/ PI 3.0))
(def ^:const QUARTER-PI (/ PI 4.0))
(def ^:const SIXTH-PI (/ PI 6.0))

(def ^:const DEG (/ 180.0 PI))
(def ^:const RAD (/ PI 180.0))

(defn degrees [theta] (* theta DEG))

(defn radians [theta] (* theta RAD))

(defn clamp
  [x min max] (if (< x min) min (if (> x max) max x)))

(defn clamp-normalized
  [x] (if (< x -1.0) -1.0 (if (> x 1.0) 1.0 x)))


(def foo-axiom '(:A))

(def foo-rules {:A '(:B :- :A :- :B)
                :B '(:A :+ :B :+ :A)})

(def foo-xf (replace foo-rules))

(defn foo-next [coll]
  (flatten (transduce foo-xf conj coll)))

(def foo-gen (iterate foo-next '(:A)))

(def foo (iterate #(flatten (transduce (replace foo-rules) conj %)) '(:A)))

(defn bar [rules axiom]
  "Returns a lazy sequence of colls, starting with axiom, where each
   subsequent coll is the result of the replacement rules applied to the
   preceding coll."
  (iterate #(flatten (transduce (replace rules) conj %)) axiom))

(def foo-bar (bar foo-rules foo-axiom))
