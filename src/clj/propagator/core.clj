(ns propagator.core
  (:use propagator.scheduler))

(def nothing :nothing)
(defn nothing? [thing] (= thing nothing))

(def contradiction :contradiction)

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

(defn- contradictory? [item] (= item contradiction))
(defn- raise-inconsistency [] (throw (Exception. "Inconsistent fact!")))

(defn- content-merge [new-info old-info]
  (cond
   (nothing? old-info) new-info
   (nothing? new-info) old-info
   (not (= new-info old-info)) contradiction
   :else old-info))

(defrecord PropagatorCell [cell-state]
  Cell
  (new-neighbor [cell nbr]
                (if (not (contains? (:neighbors @cell-state) nbr))
                  (cell-add-neighbor cell-state nbr)))
  (add-content [cell increment]
               (let [current-val (content cell)
                     answer (content-merge current-val increment)]
                 (cond
                  (= answer current-val) :ok
                  (contradictory? answer) (raise-inconsistency)
                  :else (cell-set-content cell-state answer))))
  (content [cell] (:content @cell-state))
  (remove-content [cell] (cell-remove-content cell-state))
  (neighbors [cell] (:neighbors @cell-state)))

(defn- cell-state-watch-function
  [_ _ {old-content :content, [old-head & old-rest :as old-neighbors] :neighbors } {new-content :content, new-neighbors :neighbors}]
  (cond
   (and (not= old-content new-content) (not (nothing? new-content)) (not (empty? old-neighbors))) (apply alert-propagators old-neighbors)
   (not= old-neighbors new-neighbors) (apply alert-propagators (take-while (partial not= old-head) new-neighbors))))

(defn make-cell
  "Create a new propagator cell, with no content and an empty set of neighbors.
   The cell will be watched, any change to it's state will cause it's
   neighbors to be notified.

   'Neighbors' are actually functions that get scheduled for evaluation at a later
   time. (See propagators.scheduler/alert-propagators)"
  []
  (PropagatorCell.
   (add-watch (ref {:content nothing :neighbors []})
              :changed
              cell-state-watch-function)))

(defn propagator
  [sources to-do]
  (doseq [cell sources]
    (new-neighbor cell to-do)))

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
