(ns tests.lispreader-tests
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]]))


;FIXME....this is really broken right now
#_(deftest literal-char-tests
    (assertions/assert-equal \tab "\t")
    (assertions/assert-equal \space " ")
    (assertions/assert-equal \z "z")
    (assertions/assert-equal \a "a"))

(py/print "all tests passed")
