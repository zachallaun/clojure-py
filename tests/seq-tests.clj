(ns tests.seq-tests
    (:require [tests.assertions :as assertions])
    (:use [tests.utils :only [deftest]]))

(deftest list-tests
    (assertions/assert-equal (list 1 2 3) (py/list [1 2 3]))
    (assertions/assert-equal (list) (py/list []))
    (assertions/assert-true (instance? clojure.lang.persistentlist/PersistentList (list 1 2 3))))

(deftest vector-tests
    (assertions/assert-equal (vector 1 2 3) (py/list [1 2 3]))
    (assertions/assert-equal (vector) (py/list []))
    (assertions/assert-true (instance? clojure.lang.persistentvector/PersistentVector (vector 1 2 3))))

(deftest cons-tests
    (assertions/assert-equal (cons 1 nil) (py/list [1]))
    (assertions/assert-equal (cons 1 [2]) (py/list [1 2]))
    (assertions/assert-equal (cons 1 (cons 2 (cons 3 nil))) (py/list [1 2 3])))

(deftest seq-tests
    (assertions/assert-equal (seq []) py/None)
    (assertions/assert-equal (seq nil) py/None))

(deftest first-tests
    (assertions/assert-equal (first [1 2]) 1)
    (assertions/assert-equal (first nil) py/None))

(deftest next-tests
    (assertions/assert-equal (next [1 2]) (py/list [2]))
    (assertions/assert-equal (next nil) py/None))
