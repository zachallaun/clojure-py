(ns clojure.examples.check_base_vm)

(defn get-vm []
 (let [cls (str (class false))]
  (if (= cls "<type 'bool'>") "Python"
   (if (= cls "class java.lang.Boolean") "JVM"
    "unknown"))))

    
(println (get-vm))

