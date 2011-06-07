(ns propagator.scheduler)

;;; Open questions
;;;  - Is a ordering on the propagator functions significant?
;;;  - Set a limit on run iterations? (Might be needed to prevent oscillations?)

;;; public api:
;;; (alert-propagators jobs)     schedule a coll of jobs
;;; (run)                        runs scheduled jobs, plus any new
;;;                              jobs created as a result

(def alerted-propagators (ref #{}))

(defn alert-propagators
  "Schedule a collection of propagators to execute at the next run"
  [& propagators]
  (dosync (commute alerted-propagators
                 (fn [prior] (apply conj prior propagators)))))

(defn clear-alerted-propagators
  "Remove all previously alerted propagators and return the set"
  []
  (dosync (ref-set alerted-propagators #{})))

(defn any-propagators-alerted? [] (not (empty? @alerted-propagators)))

(defn run
  "Execute all propagators currently activated. Continue running until no propagators remain"
  ([] (run 0))
  ([iter] (let [alerted @alerted-propagators]
        (dosync (clear-alerted-propagators)
                (doseq [p alerted]
                  (p))))
      (if (any-propagators-alerted?)
        (recur (inc iter))
        iter
        )))
