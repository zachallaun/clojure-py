(ns tests.utils)


(defmacro deftest [name & body]
    `((~'fn ~name [] ~@body)))
