(ns bifocals.lens-test
  (:require
   [bifocals.lens :as l]
   [clojure.test :refer [deftest is]]
   [clojure.java.io :as io]
   [clojure.edn :as edn]))


(deftest id-test
  (is (= {:x 4} (l/view (comp l/id l/id) {:x 4})))
  (is (= 4 (l/over l/id inc 3)))
  (is (= 3 (l/set l/id 4 3)))
  )

(deftest kv-test
  (is (= (l/view (comp (l/key :x) (l/key :y)) {:x {:y 4}}) 4))
  (is (= (l/over (l/key :x) inc {:x 3}) {:x 4}))
  (is (= (l/set (l/key :x) {:x 4} 3) {:x 3}))
  (is (= (l/set (comp (l/key :x) (l/key :y)) {} 5) {:x {:y 5}}))
  )

(deftest +>-test

  (is (= (l/view (comp l/map (l/+> (l/key :y) (l/key :x)))
                 [{:x 1 :y 2} {:x 3 :y 4}])
         '[(2 1) (4 3)]))

  (is (= (l/over (comp l/map (l/+> (l/key :y) (l/key :x)))
                 inc
                 [{:x 1 :y 2} {:x 3 :y 4}])
         [{:x 2, :y 3} {:x 4, :y 5}]))

  (is (= (l/over (l/+> (l/key :x) (l/key :y) (l/key :z))
                 inc
                 {:x 1 :y 2 :z 3})
         {:x 2, :y 3, :z 4})) 

  )

;; (clojure.test/run-tests)
