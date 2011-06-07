(ns propagator.core
  (:use propagator.scheduler))

(def nothing [:the-nothing])

(defn nothing? [thing] (= thing nothing))

(defprotocol Cell
  (new-neighbor [cell nbr])
  (add-content [cell increment])
  (content [cell])
  (remove-content [cell])
  (neighbors [cell]))

(declare make-cell)

(defmacro mutator
  [name argv & body]
  (let [carg `cell#]
    `(defn- ~name ~(into [carg] argv)
       (dosync (alter ~carg (fn [~'state] ~@body))))))

(mutator cell-set-content [content] (assoc state :content content))
(mutator cell-add-neighbor [neighbor] (assoc state :neighbors (cons neighbor (:neighbors state))))
(mutator cell-remove-content [] (assoc state :content nothing))

(defrecord PropagatorCell [cell-state]
  Cell
  (new-neighbor [cell nbr]
                (if (not (contains? (:neighbors @cell-state) nbr))
                   (cell-add-neighbor cell-state nbr)))
  (add-content [cell increment]
               (cond
                (nothing? increment) :ok
                (nothing? (:content @cell-state)) (cell-set-content cell-state increment)
                :else (if (not (= (:content @cell-state) increment)) (throw (Exception. "Inconsistent fact!")))))
  (content [cell] (:content @cell-state))
  (remove-content [cell] (cell-remove-content cell-state))
  (neighbors [cell] (:neighbors @cell-state)))

(defn make-cell [] (PropagatorCell. (ref {:content nothing :neighbors []})))

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
  (fn [& cells]
    (let [output (last cells)
          inputs (butlast cells)
          lifted-f (lift-to-cell-contents f)]
      (propagator inputs
                  #(add-content output
                                (apply lifted-f (map content inputs)))))))

(def adder (function->propagator-constructor +))
(def subtractor (function->propagator-constructor -))
(def multiplier (function->propagator-constructor *))
(def divider (function->propagator-constructor /))
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
