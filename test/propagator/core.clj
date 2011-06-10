(ns propagator.core.test
  (:use propagator.core
        propagator.scheduler
        clojure.test))

(defn- dummy-propagator
  ([input output] #(add-content output true)))

(deftest cells
  (testing "creation"
    (is (not (identical? (make-cell) (make-cell))))
    (is (nothing? (content (make-cell)))))
  
  (testing "content change"
    (is (identical? :foo 
                    (let [cell (make-cell)]
                      (add-content cell :foo)
                      (content cell))))
    (is (thrown? Exception (let [cell (make-cell)]
                             (add-content cell :foo)
                             (add-content cell :bar)))))
  
  (testing "neighbor change"
    (is (do
          (let [in (make-cell)
                out (make-cell)
                prop (dummy-propagator in out)]
            (new-neighbor in prop)
            (not (empty? @alerted-propagators)))))
    (is (do
          (let [in (make-cell)
                out (make-cell)
                prop (dummy-propagator in out)]
            (new-neighbor in prop)
            (run)
            (content out))))))
