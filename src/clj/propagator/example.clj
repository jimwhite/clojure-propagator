(ns propagator.example
  (:use propagator.core
        propagator.scheduler
        clojure.test))

(defn has-content?
  ([c] (not (nothing? (content c))))
  ([c & cs] (and (has-content? c) (apply has-content? cs))))

(defn adder-n
  [in1 in2 out]
  (propagator [in1 in2 out]
              #(cond
                (has-content? in1 in2) (add-content out (+ (content in1) (content in2)))
                (has-content? in1 out) (add-content in2 (- (content out) (content in1)))
                (has-content? in2 out) (add-content in1 (- (content out) (content in2))))))

(defn make-nway-adder
  [& vals]
  (let [cells (map make-cell vals)]
    (apply adder-n cells)
    cells))

(deftest multiway
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

