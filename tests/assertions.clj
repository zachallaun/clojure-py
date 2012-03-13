(ns tests.assertions)


(defn fail [& msgs]
  (throw (py/AssertionError (apply str msgs))))


(defn assert-true [val]
    (if val true
            (fail val " is not true")))

(defn assert-false [val]
    (if val (fail val " is not false")
            false))

(defn assert-greater [x y]
    (if (py.bytecode/COMPARE_OP ">" x y)
        true
        (fail x " is not greater than " y)))

(defn assert-lesser [x y]
    (if (py.bytecode/COMPARE_OP "<" x y)
        true
        (fail x " is not less than " y)))

(defn assert-equal [x y]
    (if (py.bytecode/COMPARE_OP "==" x y)
        true
        (fail x " does not equal " y)))
