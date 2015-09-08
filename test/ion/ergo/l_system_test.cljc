(ns ion.ergo.l-system-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [ion.ergo.l-system :refer :all]))

(deftest rewrite-test
  (let [rules {:A [:B :A :B]}]
    (is (= (rewrite-basic rules :A) [:B :A :B]))
    (is (= (rewrite-basic rules :B) [:B]))
    (is (= (rewrite-basic rules :C) [:C]))))

(deftest basic-system-test
  (let [axiom [0]
        rules {0 [0 1] 1 [0]}
        system (basic-system axiom rules)]
    (is (= 144 (-> system (nth 10) count)))))

(deftest basic-functional-system-test
  (let [axiom #(vec [(rand-int 2)])
        rules {0 [0 1] 1 [0]}
        system (basic-functional-system axiom rules)
        size (-> system (nth 10) count)]
    (is (or (= 89 size) (= 144 size)))))

(deftest generational-system-test
  (let [axiom [0]
        rules {0 (fn [g] [0 (rand-int (+ g 5)) 1])
               1 [0]}
        system (generational-system axiom rules)]
    (is (<= 5 (-> system (nth 2) count)))))
