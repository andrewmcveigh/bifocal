(ns bifocal.combinator
  (:refer-clojure :exclude [dissoc select-keys])
  (:require
   [bifocal.functor :refer [ffilter -fmap fmap]]
   [bifocal.lens :as l]))

(alias 'c 'clojure.core)

(defn unstable-set [_ _]
  (throw (Exception. "This lens is unstable in the set direction")))

(defn dup [n]
  (fn [f]
    (fn
      ([s] (repeat n s))
      ([s g] (fmap g s)))))

(defn promote-key
  [k]
  (l/lens
   (fn [s]
     (let [m (get s k)]
       (if (map? m)
         (-> s (merge m) (c/dissoc k))
         (throw (Exception. (format "Value at key %s is not a map: %s" k m))))))
   unstable-set))

(defn dissoc [& ks]
  (l/lens
   (fn [s] (apply c/dissoc s ks))
   l/id-setter))

(defn select-keys [& ks]
  (l/lens
   (fn [s] (c/select-keys s ks))
   l/id-setter))

(defn grow [f]
  (l/lens
   (fn [s]
     (let [x (f s)
           n (count x)]
       (repeat n s)))
   -fmap))

(defn shrink [f]
  (l/lens
   (fn [s]
     (map (comp first val) (group-by f s)))
   l/id-setter))

;; (l/view (promote-key :c) {:a 1 :b 2 :c {:e 2} :d "d"})
