(ns bifocal.lens-test
  (:require
   [bifocal.lens :as l]
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

(deftest categorize-test
  (is (= (l/view (comp l/map
                       (l/categorize {:k :type} {:k :type})
                       (l/+> (l/key :x) (l/key :y)))
                 [{:type "x" :x 1 :y 2} {:type "y" :x 3 :y 4}])
         '({:category {:k "x"}, :value [1 2]} {:category {:k "y"}, :value [3 4]}))))

(deftest prism-test
  (is (= 1 (l/view (l/prism [:x (l/key :x)] [:y (l/key :y)]) {:x 1})))
  (is (= 2 (l/view (l/prism [:x (l/key :x)] [:y (l/key :y)]) {:y 2}))))

(deftest nth-test
  (is (= 1 (l/view (l/nth 0) [1 2 3])))
  (is (= 2 (l/view (l/nth 1) [1 2 3])))
  (is (= 3 (l/view (l/nth 2) [1 2 3])))
  (is (= [2 2 3] (l/over (l/nth 0) inc [1 2 3])))
  (is (= [1 3 3] (l/over (l/nth 1) inc [1 2 3])))
  (is (= [1 2 4] (l/over (l/nth 2) inc [1 2 3]))))

;; (clojure.test/run-tests)
