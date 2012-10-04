(ns snippets.phil)
;(ns seajure.bbq)

(defn a []
  (+ (->> #'a meta :ns str count (+ 40) str reverse
       (apply str) read-string (* 1000))
    400 (int (Math/sqrt (apply * (take 6 (repeat 2)))))))

(defn b []
  (ffirst (filter #(= "Same as (first (next x))"
                     (-> % val meta :doc))
            (ns-publics 'clojure.core))))

(defn c
  ([] (c #{"Ave" "Street" "Blvd" "Junction"}))
  ([s] (or (and (empty? (disj s (first s))) (first s))
         (c (disj s (last (sort-by count s)))))))

(defn d []
  (let [f (fn [c]
    (->> (ns-interns *ns*)
      (sort-by c)
      first key name last))
        one (fn [f] (f key))
        two (fn [f] (f (comp not :private meta val)))]
    (.toUpperCase
      (apply str ((juxt one two) f)))))

(defn- e []
  (/ (+ 24 426) 10))

(defn -main []
  ((ns-resolve (doto 'clojure.string require) 'join) " "
    ((apply juxt (for [i (range 4)]
                   (-> i (+ 97) char str symbol resolve))))))

(print (-main))