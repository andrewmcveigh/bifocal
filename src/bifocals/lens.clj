(ns bifocals.lens
  (:refer-clojure :exclude [key map set])
  (:require
   [bifocals.functor :refer [-fmap fmap]]
   [clojure.set :as set]))

;; forall g . Functor g => (a -> g a) -> g b

(alias 'c 'clojure.core)
;; data Lens s a = Lens (s -> (a -> s, a))

;; lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
(defn lens [get set]
  ;; f :: s -> a
  ;; g :: s -> a -> s
  (fn [f]
    (fn
      ([s] (f (get s)))
      ([s g] (set s #(f % g))))))

(deftype Value [m v]
  bifocals.functor.Functor
  (-fmap [Fa f] (Value. m (f v))))

(defn value [v] (Value. {} v))

(def v (lens #(.-v %) -fmap))

(defn const [s _] s)

(defn id-setter [s g] (g s))


(def id (lens identity id-setter))

(defn key [k] (lens k (fn [s f] (update s k f))))

(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))

(defn traversal [get set]
  (fn [f]
    (fn
      ([s] (fmap f (get s)))
      ([s g] (fmap (fn [x] (set x #(f % g))) s)))))

(def map (traversal sequence id-setter))

;;    view :: Lens' a b -> a -> b
(defn view [lens a]
  ((lens identity) a))

;;    over :: Lens' a b -> (b -> b) -> a -> a
(defn over [lens f a]
  (let [setter (lens id-setter)]
    (setter a f)))

;;    set :: Lens' a b -> b -> a -> a
(defn set [lens s a]
  (over lens (fn [_] a) s))

(defn +>
  "Combine `lenses` in parallel, view & update"
  [& lenses]
  (reduce (fn [init x]
            (lens
             (fn [s] (conj (view init s) (view x s)))
             (fn [s f]
               (->> s (over init f) (over x f)))))
          (lens (constantly []) const)
          lenses))

(defn +>>
  "Combine `lenses` in parallel, view & set"
  [& lenses]
  (reduce (fn [init [i x]]
            (lens
             (fn [s] (conj (view init s) (view x s)))
             (fn [s f]
               (->> s
                    (over init f)
                    (over x (c/comp #(nth % i) f))))))
          (lens (constantly []) const)
          (map-indexed vector lenses)))

(defn a->b [a->b b->a]
  (lens a->b (fn [a f] (b->a (f (a->b a))))))

(defn rename-key [k l]
  (a->b #(set/rename-keys % {k l}) #(set/rename-keys % {l k})))

(defn categorize [ct-m un-m]
  (fn [f]
    (fn
      ([s]
       (let [c (->> ct-m (c/map (fn [[k ct-f]] [k (ct-f s)])) (into {}))]
         {:category c :value (f s)}))
      ([s f]))))
