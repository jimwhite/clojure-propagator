(ns propagator.core
  (:use propagator.scheduler))

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
(mutator cell-add-neighbor [neighbor] (assoc state :neighbors (conj (:neighbors state) neighbor)))
(mutator cell-remove-content [] (dissoc state :content))

(defn- contradictory? [item] (= item :contradiction))
(defn- raise-inconsistency [] (throw (Exception. "Inconsistent fact!")))

(defn- content-merge [new-info old-info]
  (cond
   (nil? old-info) new-info
   (nil? new-info) old-info
   (not (= new-info old-info)) :contradiction
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

(defn items-before
  [coll sentinel]
  (let [pred (partial not= sentinel)]
    (take-while pred coll)))

(defn- cell-neighbor-watch
  "When the list of neighbors changes, alert the added neighbors so they receive
  the cell's content on the next scheduler run."
  [_ _ {[old-head & _ :as old-neighbors] :neighbors} {new-neighbors :neighbors}]
  (if (not= old-neighbors new-neighbors)
    (alert-propagators (items-before new-neighbors old-head))))

(defn- cell-content-watch
  "When the content of a cell changes, alert the cell's neighbors so they receive
   the content from the cell."
  [_ _ {old-content :content} {neighbors :neighbors, new-content :content}]
  (if (and (not= old-content new-content) (not (empty? neighbors)))
    (alert-propagators neighbors)))

(defn make-cell
  "Create a new propagator cell, with no content and an empty set of neighbors.
   The cell will be watched, any change to it's state will cause it's
   neighbors to be notified.

   'Neighbors' are actually functions that get scheduled for evaluation at a later
   time. (See propagators.scheduler/alert-propagators)"
  []
  (PropagatorCell.
   (doto (ref {:neighbors []})
     (add-watch :neighbors cell-neighbor-watch)
     (add-watch :content cell-content-watch))))

(defn propagator
  [sources to-do]
  (doseq [cell sources]
    (new-neighbor cell to-do))
  (alert-propagators [to-do]))

(defn lift-to-cell-contents
  [f]
  (fn [& args] (if (some nil? args) nil (apply f args))))

(defn function->propagator-constructor
  [f]
  (fn [& cells]
    (let [output (last cells)
          inputs (butlast cells)
          lifted-f (lift-to-cell-contents f)]
      (println cells)
      (propagator inputs
                  #(do
                     (println output)
                     (println lifted-f)
                     (add-content output
                                  (apply lifted-f (map content inputs))))))))

(def adder (function->propagator-constructor +))
(def subtractor (function->propagator-constructor -))
(def multiplier (function->propagator-constructor *))
(def divider (function->propagator-constructor /))
(defn constant [val] (function->propagator-constructor (fn [] val)))

(defn conditional [p if-true if-false output]
  (propagator '(p if-true if-false)
              (fn []
                (let [pred (content p)]
                  (if (nil? pred)
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
        (fn [] (if (some (comp not nil?) (map content neighbors))
                (to-build)))])
  (propagator neighbors test))
