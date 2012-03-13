(ns tests.lispreader-tests
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]]))


(deftest literal-char-tests
    (assertions/assert-equal \tab "\t")
    (assertions/assert-equal \space " ")
    (assertions/assert-equal \z "z")
    (assertions/assert-equal \a "a"))

(py/print "all tests passed")
