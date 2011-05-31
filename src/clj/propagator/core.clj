(ns propagator.core
  (:use clojure.contrib.error-kit))

(def nothing [:the-nothing])

(defn nothing? [thing] (= thing nothing))

(defprotocol Cell
  (new-neighbor [cell new-neighbor])
  (add-content [cell increment])
  (content [cell])
  (neighbors [cell]))

(declare make-cell)

(defrecord PropagatorCell [content neighbors]
  Cell
  (new-neighbor [cell new-neighbor]
                 (if (not (contains? neighbors new-neighbor))
                   (make-cell content (conj neighbors new-neighbor))))
  (add-content [cell increment]
               (cond
                (nothing? increment) :ok
                (nothing? content) (make-cell content neighbors)
                :else (if (not (= content increment)) (throw (Exception. "Inconsistent fact!")))))
  (content [cell] content)
  (neighbors [cell] neighbors))

(defn make-cell
  ([] (PropagatorCell. nothing #{}))
  ([content neighbors] (PropagatorCell. content neighbors)))
