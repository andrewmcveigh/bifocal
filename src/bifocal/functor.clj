(ns bifocal.functor)

(defprotocol Functor
  (-fmap [Fa f]))

(defn fmap [f fa] (-fmap fa f))

(extend-protocol Functor
  clojure.lang.IPersistentMap
  (-fmap [Fa f] (into {} (map (fn [[k v]] [k (f v)]) Fa)))
  clojure.lang.IPersistentVector
  (-fmap [Fa f] (mapv f Fa))
  clojure.lang.IPersistentSet
  (-fmap [Fa f] (set (map f Fa)))
  clojure.lang.IPersistentList
  (-fmap [Fa f] (map f Fa))
  clojure.lang.ISeq
  (-fmap [Fa f] (map f Fa)))
