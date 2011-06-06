(ns propagator.core
  (:use clojure.contrib.error-kit
        propagator.scheduler))

(def nothing [:the-nothing])

(defn nothing? [thing] (= thing nothing))

(defprotocol Cell
  (new-neighbor [cell new-neighbor])
  (add-content [cell increment])
  (content [cell])
  (neighbors [cell]))

(declare make-cell)

(defmacro mutator
  [name argv & body]
  (let [carg `cell#]
    `(defn ~name ~(into [carg] argv)
       (dosync (alter ~carg (fn [~'state] ~@body))))))

(mutator set-content [content] (assoc state :content content))
(mutator add-neighbor [neighbor] (assoc state :neighbors (cons neighbor (:neighbors state))))

(defrecord PropagatorCell [content neighbors]
  Cell
  (new-neighbor [cell new-neighbor]
                 (if (not (contains? neighbors new-neighbor))
                   (make-cell content (conj neighbors new-neighbor))))
  (add-content [cell increment]
               (cond
                (nothing? increment) :ok
                (nothing? content) (make-cell increment neighbors)
                :else (if (not (= content increment)) (throw (Exception. "Inconsistent fact!")))))
  (content [cell] content)
  (neighbors [cell] neighbors))

(defn make-cell
  ([] (PropagatorCell. nothing #{}))
  ([content neighbors] (PropagatorCell. content neighbors)))

(defn propagator
  [neighbors to-do]
  (doseq [cell neighbors]
    (new-neighbor cell to-do))
  (alert-propagators to-do))

(defn lift-to-cell-contents
  [f]
  (fn [& args] (if (some nothing? args) nothing (apply f args))))

(defn function->propagator-constructor
  [f]
  (fn [cells]
    (let [output (first cells)
          inputs (rest cells)
          lifted-f (lift-to-cell-contents f)]
      (propagator inputs
                  #(add-content output
                                (apply lifted-f (map content inputs)))))))

(defn adder [] (function->propagator-constructor +))
(defn subtractor [] (function->propagator-constructor -))
(defn multiplier [] (function->propagator-constructor *))
(defn divider [] (function->propagator-constructor /))
(defn constant [val] (function->propagator-constructor (fn [] val)))

(defn conditional [p if-true if-false output]
  (propagator '(p if-true if-false)
              (fn []
                (let [pred (content p)]
                  (if (nothing? pred)
                    :done
                    (add-content output
                                 (if pred
                                   (content if-true)
                                   (content if-false))))))))

(defn switch [pred if-true output]
  (conditional pred if-true (make-cell) output))

(defn compound-propagator
  [neighbors to-build]
  (let [test  
        (fn [] (if (some (comp not nothing?) (map content neighbors))
                (to-build)))])
  (propagator neighbors test))
