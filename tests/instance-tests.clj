(ns tests.instance-tests
    (:require [tests.assertions :as assertions])
    (:use [tests.utils :only [deftest]]))

(deftest instance-tests
    (assertions/assert-true (instance? (py/type 5) 3))
    (assertions/assert-true (instance? (py/type "") "foo"))
    (assertions/assert-true (instance? (py/type 1.0) 2.718))
    (def PatternType (py/type (re/compile "")))
    (assertions/assert-false (instance? PatternType "")))


