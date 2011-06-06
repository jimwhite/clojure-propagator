(ns propagator.example
  (:use propagator.core))

(defn heron-step [x g h]
  (compound-propagator [x g]
                       (fn []
                         (let [x-div-g (make-cell)
                               g+x-div-g (make-cell)
                               two (make-cell)]
                           (divider x g x-div-g)
                           (adder g x-div-g g+x-div-g)
                           ((constant 2) two)
                           (divider g+x-div-g two h)))))
