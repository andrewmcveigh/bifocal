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

(deftest promote-only-test
  (is (= (l/view (comb/promote-only :c :e) {:a 1 :b 2 :c {:e 2 :f 7} :d "d"})
         {:a 1, :b 2, :d "d", :e 2}))
  (is (= (l/view (comp l/id
                       (comb/promote-only :c :e)
                       l/id)
                 {:a 1 :b 2 :c {:e 2 :f 7} :d "d"})
         {:a 1, :b 2, :d "d", :e 2})))

(deftest demote-keys-test
  (is (= (l/view (comb/demote-keys :f :a :b) {:a 1 :b 2 :c {:e 2} :d "d"})
         {:f {:a 1 :b 2} :c {:e 2} :d "d"}))
  (is (= (l/over (comb/demote-keys :f :a :b)
                 identity
                 {:f {:a 1 :b 2} :c {:e 2} :d "d"})
         {:a 1 :b 2 :c {:e 2} :d "d"})))

(deftest dissoc-test
  (is (= (l/view (comb/dissoc :c) {:a 1 :b 2 :c {:e 2} :d "d"})
         {:a 1, :b 2, :d "d"})))

(deftest select-keys-test
  (is (= (l/view (comb/select-keys :a :b :d) {:a 1 :b 2 :c {:e 2} :d "d"})
         {:a 1, :b 2, :d "d"})))

(deftest grow-test
  (is (= (l/view (comb/grow :a)
                 '({:a #{"type-1" "type-2"} :b 2 :c {:e 2} :d "d"}))
         '({:a #{"type-1" "type-2"}, :b 2, :c {:e 2}, :d "d"}
           {:a #{"type-1" "type-2"}, :b 2, :c {:e 2}, :d "d"}))))

(deftest shrink-test
  (is (= (l/view (comb/shrink :a)
                 '({:a #{"type-1" "type-2"}, :b 2, :c {:e 2}, :d "d"}
                   {:a #{"type-1" "type-2"}, :b 2, :c {:e 2}, :d "d"}))
         '({:a #{"type-1" "type-2"} :b 2 :c {:e 2} :d "d"}))))

(def A {:a "a"
        :b "b"
        :c {:e 7
            :f [{:g #{:a} :h 9 :i {:j "j" :k '[a b c]}}
                {:g #{:b :c} :h 10 :i {:j "j" :k '[a b c]}}
                {:g #{:d} :h 11 :i {:j "j" :k '[a b c]}}]}})

(deftest cross-sequence-test
  (is (= (l/view (comp (comb/promote-key :c)
                       (comb/promote-sequence :f)
                       (comb/split :g #(assoc % :g %2))
                       l/map
                       (comb/dissoc-in [:i] :k))
                 A)
         '({:g :a :h 9 :i {:j "j"} :a "a" :b "b" :e 7}
           {:g :c :h 10 :i {:j "j"} :a "a" :b "b" :e 7}
           {:g :b :h 10 :i {:j "j"} :a "a" :b "b" :e 7}
           {:g :d :h 11 :i {:j "j"} :a "a" :b "b" :e 7}))))

;; (clojure.test/run-tests)
