(ns tests.lispreader-tests)


(defn assert-true [val]
    (if val true 
            (throw (py/AssertionError (str val " is not true")))))

(defn assert-false [val]
    (if val (throw (py/AssertionError (str val " is not false")))
            false))

(defn assert-greater [x y]
    (if (py.bytecode/COMPARE_OP ">" x y)
        true
        (throw (py/AssertionError (str x " is not greater than " y)))))

(defn assert-lesser [x y]
    (if (py.bytecode/COMPARE_OP "<" x y)
        true
        (throw (py/AssertionError (str x " is not greater than " y)))))

(defn assert-equal [x y]
    (if (py.bytecode/COMPARE_OP "==" x y)
        true
        (throw (py/AssertionError (str x " does not equal " y)))))


(defmacro deftest [name & body]
    `((~'fn ~name [] ~@body)))


(deftest literalchar-tests
    (assert-equal \tab "\t")
    (assert-equal \space " ")
    (assert-equal \z "z")
    (assert-equal \a "a"))




