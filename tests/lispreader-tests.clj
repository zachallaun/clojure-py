(ns tests.lispreader-tests
    (:require [tests.assertions :as a])
    (:use [tests.utils :only [deftest]]))

(deftest literal-char-tests
    (a/assert-equal \tab "\t")
    (a/assert-equal \space " ")
    (a/assert-equal \z "z")
    (a/assert-equal \a "a"))

(deftest syntax-quote-tests
  (a/assert-equal `() ())
  (a/assert-equal `map 'clojure.core/map)
  (a/assert-equal `if 'clojure.core/if)
  (a/assert-equal `-> 'clojure.core/->)
  (a/assert-equal `recur 'recur)
  (a/assert-equal `quote 'quote)
  (a/assert-equal `deftest 'tests.utils/deftest))
