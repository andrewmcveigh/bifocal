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

(defn dissoc [& ks]
  (l/lens
   (fn [s] (apply c/dissoc s ks))
   l/id-setter))

(defn dissoc-in [path & ks]
  (l/lens
   (fn [s] (update-in s path #(apply c/dissoc % ks)))
   l/id-setter))

(defn select-keys [& ks]
  (l/lens
   (fn [s] (c/select-keys s ks))
   l/id-setter))

(defn promote-key [k]
  (l/lens
   (fn [s]
     (let [m (get s k)]
       (if (map? m)
         (-> s (merge m) (c/dissoc k))
         (throw (Exception. (format "Value at key %s is not a map: %s" k m))))))
   unstable-set))

(defn in [path lens]
  (l/lens
   (fn [s]
     (update-in s path (partial l/view lens)))
   unstable-set))

(defn promote-only [k & ks]
  (comp (in [k] (apply select-keys ks))
        (promote-key k)))

(defn promote-sequence [branch]
  (l/lens
   (fn [s]
     (let [coll (get s branch)
           s' (c/dissoc s branch)]
       (map #(merge % s') coll)))
   unstable-set))

(defn demote-keys [k & ks]
  (l/lens
   (fn [s]
     (let [m (c/select-keys s ks)]
       (-> (apply c/dissoc s ks)
           (assoc k m))))
   (fn [s f]
     (let [m (get s k)]
       (if (map? m)
         (f (-> s (merge m) (c/dissoc k)))
         (throw (Exception. (format "Value at key %s is not a map: %s" k m))))))))

(defn grow [f]
  (l/lens
   (fn [s]
     (mapcat (fn [s]
               (let [x (f s)
                     n (count x)]
                 (repeat n s)))
             s))
   -fmap))

(defn shrink [f]
  (l/lens
   (fn [s]
     (map (comp first val) (group-by f s)))
   l/id-setter))

(defn split [f g]
  (l/lens
   (fn [s]
     (mapcat (fn [s]
               (let [x (f s)]
                 (map #(g s %) x)))
             s))
   -fmap))

