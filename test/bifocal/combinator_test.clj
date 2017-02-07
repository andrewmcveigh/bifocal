(ns bifocal.combinator-test
  (:require
   [bifocal.combinator :as comb]
   [bifocal.lens :as l]
   [clojure.test :refer [deftest is]]))

(deftest dup-test
  (is (= (l/view (comb/dup 4) {:a 1}) '({:a 1} {:a 1} {:a 1} {:a 1}))))

(deftest promote-key-test
  (is (= (l/view (comb/promote-key :c) {:a 1 :b 2 :c {:e 2} :d "d"})
         {:a 1, :b 2, :d "d", :e 2})))

(deftest dissoc-test
  (is (= (l/view (comb/dissoc :c) {:a 1 :b 2 :c {:e 2} :d "d"})
         {:a 1, :b 2, :d "d"})))

(deftest select-keys-test
  (is (= (l/view (comb/select-keys :a :b :d) {:a 1 :b 2 :c {:e 2} :d "d"})
         {:a 1, :b 2, :d "d"})))

(deftest grow-test
  (is (= (l/view (comb/grow :a)
                 {:a #{"type-1" "type-2"} :b 2 :c {:e 2} :d "d"})
         '({:a #{"type-1" "type-2"}, :b 2, :c {:e 2}, :d "d"}
           {:a #{"type-1" "type-2"}, :b 2, :c {:e 2}, :d "d"}))))

(deftest shrink-test
  (is (= (l/view (comb/shrink :a)
                 '({:a #{"type-1" "type-2"}, :b 2, :c {:e 2}, :d "d"}
                   {:a #{"type-1" "type-2"}, :b 2, :c {:e 2}, :d "d"}))
         '({:a #{"type-1" "type-2"} :b 2 :c {:e 2} :d "d"}))))
