(ns tests.utils)

(def metadata {:test true})

(defmacro deftest [name & body]
    `(defn ~name "test" ~metadata [] ~@body))
