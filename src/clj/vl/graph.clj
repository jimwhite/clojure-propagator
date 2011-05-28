(ns
  #^{:author "Michael Nygard",
     :doc "Simple graph data structure. A weak version of clojure.contrib.graph from Jeffrey Straszheim."}
  vl.graph)

(defstruct directed-graph
  :nodes       ; The nodes of the graph, a collection
  :neighbors)  ; A function that, given a graph and node returns a collection
               ; neighbor nodes.

(defn get-neighbors
  "Get the neighbors of a node."
  [g n]
  ((:neighbors g) g n))

(defn get-nodes [g] (:nodes g))

(defn add-node
  "Add a node to a graph"
  [g n]
  (assoc g :nodes (conj (get-nodes g) n)))

(defn replace-node [g old-node new-node]
  (assoc g :nodes (replace {old-node new-node} (get-nodes g))))

(defn replace-nodes [g old-node-coll new-node-coll]
  (assoc g :nodes (replace (zipmap old-node-coll new-node-coll) (get-nodes g))))
