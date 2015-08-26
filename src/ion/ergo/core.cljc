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

