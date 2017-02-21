(ns bifocal.lens
  (:refer-clojure :exclude [cond filter flatten key map meta nth set])
  (:require
   [bifocal.functor :refer [ffilter -fmap fmap]]
   [clojure.set :as set]
   [clojure.spec :as s]))

(alias 'c 'clojure.core)

(deftype Value [m v]
  clojure.lang.IPersistentCollection
  (cons [_ o] (Value. m (.cons v o)))
  (empty [_] (Value. m (.empty v)))
  (equiv [_ other ]
    (if (instance? Value other)
      (and (= m (.-m other))
           (= v (.-v other)))
      false))
  clojure.lang.ISeq
  (first [_] (Value. m (.first v)))
  (next [_] (Value. m (.next v)))
  (more [_] (Value. m (.more v)))
  clojure.lang.ILookup
  (valAt [_ k] (Value. m (.valAt v k)))
  (valAt [_ k not-found] (Value. m (.valAt v k not-found))))

(defmethod print-method Value [x w]
  (.write w (format "#<Value %s %s>"
                    (pr-str (.-v x))
                    (apply pr-str (apply concat (.-m x))))))

(defn value
  ([m v] (Value. m v))
  ([v] (value {} v)))

(s/def ::get-f (s/fspec :args (s/cat :s any?) :ret any?))
(s/def ::upd-f (s/fspec :args (s/cat :s any? :f fn?) :ret any?))
(s/def ::g     (s/fspec :args (s/cat :x any?) :ret any?))
(s/def ::inner (s/or :arity-1 (s/fspec :args (s/cat :s any?))
                     :arity-2 (s/fspec :args (s/cat :s any? :g ::g))))
(s/def ::lens  (s/fspec :args (s/cat :f ::inner) :ret ::inner))
(s/fdef lens
  :args (s/cat :get-f ::get-f :upd-f ::upd-f)
  :ret  ::lens)

;;; forall g . Functor g => (a -> g a) -> g b
;;: data Lens s a = Lens (s -> (a -> s, a))
;; lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
(defn lens [get set]
  ;; f :: s -> a
  ;; g :: s -> a -> s
  (fn [f]
    (fn
      ([s] (f (get s)))
      ([s g] (set s #(f % g))))))

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

(def filter-map (traversal (partial remove nil?) id-setter))

(defn filter [pred]
  (fn [f]
    (fn
      ([s] (fmap f (ffilter pred s)))
      ([s g] (->> s
                  (ffilter pred)
                  (fmap (fn [x] (f x g))))))))

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
                    (over x (c/comp #(c/nth % i) f))))))
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

(defn meta [f]
  (lens
   (fn [s]
     (if (instance? Value s)
       (vary-meta s merge (f s))
       (value (f s) s)))
   id-setter))

(defn cond [[pred lens-a] & pred-lenses]
  (let [pred-lenses (conj pred-lenses [pred lens-a])]
    (lens
     (fn [s]
       (some (fn [[p l]] (when (p s) (view l s))) pred-lenses))
     (fn [s f]
       (some (fn [[p l]] (when (p s) (over l f s))) pred-lenses)))))

(defn ? [pred a]
  (lens
   (fn [s]
     (when (pred s) (view a s)))
   (fn [s f]
     (when (pred s) (over a f s)))))

(defn maybe [lens]
  (? (complement nil?) lens))

(defn nth [index]
  (lens (fn [s] (c/nth s index))
        (fn [s f]
          (let [v? (vector? s)
                coll (if v? s (vec s))
                coll-into (empty s)
                updated (update coll index f)]
            (if v? updated (into coll-into updated))))))

(defn tuple [combinator & lenses]
  (->> lenses
       (map-indexed (fn [i lens] (comp (nth i) lens)))
       (apply combinator)))

(defn flat-map [f coll]
  (let [mapped (c/map f coll)]
    (if (every? sequential? mapped)
      (apply concat mapped)
      mapped)))

(defn traverse [get set]
  (fn [f]
    (fn
      ([s] (flat-map f (get s)))
      ([s g] (flat-map (fn [x] (set x #(f % g))) s)))))

(def flatten
  (traverse sequence id-setter))

;; duplication lens? tagging?
;; Costate Comonad Coalgebras
