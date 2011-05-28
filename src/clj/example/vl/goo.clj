(ns example.vl.goo
  (:use [vl shape graph])
  (:import [com.sun.org.apache.bcel.internal Repository]
           [com.sun.org.apache.bcel.internal.classfile Field Method]
           [com.sun.org.apache.bcel.internal.generic ConstantPoolGen MethodGen]))

(def java-class (memoize (fn [classname] (Repository/lookupClass (Class/forName classname)))))
(def constant-pool (memoize (fn [classname] (ConstantPoolGen. (.getConstantPool (java-class classname))))))

(defn java-methods [classname] (.getMethods (java-class classname)))
(defn java-fields [classname] (.getFields (java-class classname)))

(defn is-public? [m] (.isPublic m))
(defn is-static? [m] (.isStatic m))
(defn is-initializer? [m] (= (.getName m) "<init>"))
(defn is-abstract? [m] (.isAbstract m))
(defn is-native? [m] (.isNative m))

(defn instance-fields [classname] (filter is-static? (java-fields classname)))
(defn instance-methods [classname] (filter #(not (or (is-static? %) (is-initializer? %))) (java-methods classname)))

(defn nonvisible-methods [classname] (remove #(is-public? %) (instance-methods classname)))

(defn public-methods [classname] (filter #(is-public? %) (instance-methods classname)))

(defmulti make-member-shape class)
(defmethod make-member-shape Method [m]  (make-shape {:text (.getName m) :x 100 :y 100 :member m}))
(defmethod make-member-shape Field [f]   (make-shape {:text (.getName f) :x 200 :y 150 :member f}))

(defn instruction-list-for [classname method]
  (iterator-seq (.iterator (.getInstructionList (MethodGen. method classname (constant-pool classname))))))

(defn instructions-for [classname method]
  (map #(. % getInstruction) (instruction-list-for classname method)))

(defn is-field-or-method-reference? [inst]
  (some #(isa? % inst) '(GETFIELD PUTFIELD INVOKESPECIAL INVOKEVIRTUAL)))

(defn field-or-method-references [classname method]
  (filter is-field-or-method-reference? (instructions-for classname method)))

(defn inst-for-this-class-p [classname constant-pool]
  (fn [inst] (= classname (.getClassName inst constant-pool))))

(defn uses-from [classname m]
  (filter (inst-for-this-class-p classname (constant-pool classname)) (field-or-method-references classname m)))

(defn edges-from-node [gr n]
  (let [member (:member n)
        classname (:classname gr)
        locator (fn [m] (first (filter #(identical? m (:member %)) (get-nodes gr))))]
    (cond
      (is-abstract? member) '()
      (is-native? member) '()
      (instance? Field member) '()
      true (map locator (uses-from classname member))
      )))

(defn graph-of-class [classname]
  (struct-map directed-graph
    :classname classname
    :fields (instance-fields classname)
    :nonvisible-methods (nonvisible-methods classname)
    :public-methods (public-methods classname)
    :nodes (map make-member-shape (concat (instance-fields classname) (nonvisible-methods classname) (public-methods classname)))
    :neighbors edges-from-node
    ))
