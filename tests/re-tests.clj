(ns tests.re-tests
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]])
    (:require [re]))

(deftest re-pattern-tests
    (def PatternType (py/type (re/compile "")))
    (assertions/assert-equal (py/type (re-pattern "foo")) PatternType)
    (assertions/assert-true (instance?  PatternType (re-pattern "foo")))
    (assertions/assert-true (instance?  PatternType (re-pattern "^(.*)(foo)(bar)+$")))
)
