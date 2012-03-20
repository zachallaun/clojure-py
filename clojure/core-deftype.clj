(ns clojure.core-deftype)

(defn parse-opts [s]
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))

(defn parse-impls [specs]
  (loop [ret {} s specs]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

(defn parse-opts+specs [opts+specs]
  (let [[opts specs] (parse-opts opts+specs)
        impls (parse-impls specs)
        interfaces (keys impls)
        methods (reduce #(assoc %1 (first %2) %2)
                         {}
                         (apply concat (vals impls)))]
    (when-let [bad-opts (seq (remove #{:no-print} (keys opts)))]
      (throw (IllegalArgumentException. (apply str "Unsupported option(s) -" bad-opts))))
    [interfaces methods opts]))

(defn debug [v]
    (py/print v)
    v)

(defn wrap-specs
    [name fields specs]
    (zipmap (map clojure.core/name (keys specs))
            (map #(prop-wrap name fields %)
                 (vals specs))))



(defmacro deftype
    [name fields & specs]
    (let [[interfaces methods] (parse-opts+specs specs)
          methods (wrap-specs name fields methods) 
          methods (if (= (count fields) 0)
                      methods
                      (assoc methods "__init__" (clojure.core/make-init fields)))]
          `(~'do (def ~name (py/type ~(.-name name)
                                      (py/tuple ~(vec (concat interfaces [py/object])))
                                      (.toDict ~methods)))
                ~@(map (fn [x] `(clojure.lang.protocol/extendForType ~x ~name))
                               interfaces))))

