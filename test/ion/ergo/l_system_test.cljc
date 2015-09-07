(ns ion.ergo.l-system-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])
                     [ion.ergo.l-system :as ls :refer [basic-system]]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
