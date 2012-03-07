(ns rpythontest)

(defn run [x]
  (when (< x 100)
        (py.bytecode/PRINT_ITEM x)
        (recur (inc x))))
      
