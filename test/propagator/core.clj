(ns propagator.core.test
  (:use propagator.core
        propagator.scheduler
        clojure.test))

(defn- dummy-propagator
  ([input output] #(add-content output true)))

(deftest cells
  (testing "creation"
    (is (not (identical? (make-cell) (make-cell))))
    (is (nil? (content (make-cell)))))
  
  (testing "content change"
    (is (identical? :foo 
                    (let [cell (make-cell)]
                      (add-content cell :foo)
                      (content cell))))
    (is (thrown? Exception (let [cell (make-cell)]
                             (add-content cell :foo)
                             (add-content cell :bar))))
    (is (nil? (let [cell (make-cell)]
                (add-content cell :foo)
                (remove-content cell)
                (content cell)))))
  
  (testing "neighbor change"
    (is (do
          (let [in (make-cell)
                out (make-cell)
                prop (dummy-propagator in out)]
            (new-neighbor in prop)
            (not (empty? @alerted-propagators)))))
    (is (= true (do
                   (let [in (make-cell)
                         out (make-cell)
                         prop (dummy-propagator in out)]
                     (new-neighbor in prop)
                     (run)
                     (content out)))))))

(deftest simples
  (testing "constants"
    (is (= 99 (let [const (make-cell)]
                ((constant 99) const)
                (run)
                (content const))))))

(deftest compounds
  (testing "arithmetic"
    (is (= 44 (let [a (make-cell)
                    b (make-cell)
                    s (make-cell)]
                (adder a b s)
                (add-content a 33)
                (add-content b 11)
                (run)
                (content s))))
    (is (= 22 (let [a (make-cell)
                    b (make-cell)
                    s (make-cell)
                    c (make-cell)
                    d (make-cell)]
                (adder a b s)
                ((constant 2) c)
                (divider s c d)
                (add-content a 33)
                (add-content b 11)
                (run)
                (do (println (content a))
                    (println (content b))
                    (println (content s))
                    (println (content c))
                    (println (content d)))
                (content d))))))
