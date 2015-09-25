(ns ion.ergo.core-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [ion.ergo.core :refer :all]))

(deftest rewrite-test
  (let [rules {:A [:B :A :B]}]
    (is (= (rewrite rules :A) [:B :A :B]))
    (is (= (rewrite rules :B) [:B]))
    (is (= (rewrite rules :C) [:C]))))

(deftest basic-system-test
  (let [axiom [0]
        rules {0 [0 1] 1 [0]}
        system (basic-rewriting-system axiom rules)]
    (is (= 144 (-> system (nth 10) count)))))

(deftest functional-system-test
  (let [axiom #(vec [(rand-int 2)])
        rules {0 [0 1] 1 [0]}
        system (functional-rewriting-system axiom rules)
        size (-> system (nth 10) count)]
    (is (or (= 89 size) (= 144 size)))))
