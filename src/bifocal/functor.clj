(ns bifocal.functor)

(defprotocol Functor
  (-fmap [Fa f])
  (-ffilter [Fa f]))

(defn fmap [f fa] (-fmap fa f))
(defn ffilter [f fa] (-ffilter fa f))

(extend-protocol Functor
  clojure.lang.IPersistentMap
  (-fmap [Fa f]
    (into {} (map (fn [[k v]] [k (f v)]) Fa)))
  (-ffilter [Fa f]
    (into {} (keep (fn [[k v]] (when (f v) [k v])) Fa)))
  clojure.lang.IPersistentVector
  (-fmap [Fa f] (mapv f Fa))
  (-ffilter [Fa f] (filterv f Fa))
  clojure.lang.IPersistentSet
  (-fmap [Fa f] (set (map f Fa)))
  (-ffilter [Fa f] (set (filter f Fa)))
  clojure.lang.IPersistentList
  (-fmap [Fa f] (map f Fa))
  (-ffilter [Fa f] (filter f Fa))
  clojure.lang.ISeq
  (-fmap [Fa f] (map f Fa))
  (-ffilter [Fa f] (filter f Fa)))
