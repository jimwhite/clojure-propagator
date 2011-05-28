(ns example.vl.sample
  (:use [vl graph shape]))

(defn sample-nodes []
  (map #(make-shape %) [{:x 955  :y 589 :text "wwp"}
                        {:x 193  :y 448 :text "med"}
                        {:x 507  :y 450 :text "pbv"}
                        {:x 441  :y 478 :text "gsp"}
                        {:x 99   :y 715 :text "omz"}
                        {:x 1021 :y 17  :text "str"}
                        {:x 793  :y 138 :text "sod"}
                        {:x 761  :y 584 :text "anv"}
                        {:x 1004 :y 191 :text "aat"}
                        {:x 829  :y 744 :text "tpw"}
                        {:x 827  :y 582 :text "ecu"}
                        {:x 886  :y 653 :text "nkr"}
                        {:x 330  :y 231 :text "fbq"}
                        {:x 66   :y 331 :text "khd"}
                        {:x 515  :y 157 :text "wuc"}
                        {:x 595  :y 233 :text "jfr"}
                        {:x 416  :y 635 :text "kkx"}
                        {:x 820  :y 195 :text "gbp"}
                        {:x 423  :y 332 :text "teg"}
                        {:x 605  :y 163 :text "htn"}
                        ]))

(defn sample-node-connections [vs]
  (fn [g n] (list (nth (cycle vs) (:x n) '()))))

(defn make-sample-graph []
  (let [ns (sample-nodes)]
    (struct directed-graph ns (sample-node-connections ns))))
