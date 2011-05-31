(ns propagator.scheduler)

;;; strategy
;;; use a ref to hold a set of propagators to activate
;;; offer some functions to manipulate the contents of the ref
;;; a propagator is a function to run
;;; a propagator may alert other propagators
;;; when run is called, take all the current activations and run them,
;;; then repeat for any new activations.

;;; Open questions
;;;  - Is a ordering on the propagator functions significant?
;;;  - Set a limit on run iterations? (Might be needed to prevent oscillations?)

;;; public api:
;;; (alert-propagators jobs)     schedule a coll of jobs
;;; (run)                        runs scheduled jobs, plus any new
;;;                              jobs created as a result
;;; (abort-process x)            terminate the run, force return of x

(def *alerted-propagators* (ref (sorted-set)))

(defn alert-propagators
  "Schedule a collection of propagators to execute at the next run"
  [& propagators]
  (dosync (commute *alerted-propagators*
                 (fn [prior] (apply conj prior propagators)))))

(defn clear-alerted-propagators
  "Remove all previously alerted propagators and return the set"
  []
  (dosync (ref-set *alerted-propagators* (sorted-set))))
