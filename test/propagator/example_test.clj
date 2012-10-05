(ns propagator.example-test
  (:use propagator.core 
        propagator.scheduler 
        propagator.example 
        clojure.test))

(deftest multiway-test
  (testing "adder with backprop"
    (is (= 21 (let [[a b s] (make-nway-adder 8 13 nothing)]
                (run)
                (content s))))
    (is (= 8 (let [[a b s] (make-nway-adder 13 nothing 21)]
               (run)
               (content b))))
    (is (= 13 (let [[a b s] (make-nway-adder nothing 8 21)]
                (run)
                (content a))))
    (is (nothing? (let [[a b s] (make-nway-adder 13 nothing nothing)]
                (run)
                (content s))))))
