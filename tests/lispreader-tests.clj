(ns tests.lispreader-tests
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]]))


;FIXME....this is really broken right now
#_(deftest literal-char-tests
    (assertions/assert-equal \tab "\t")
    (assertions/assert-equal \space " ")
    (assertions/assert-equal \z "z")
    (assertions/assert-equal \a "a"))

(deftest syntax-quote-tests
  (assertions/assert-true (= `() ()))
  (assertions/assert-true (= `map 'clojure.core/map))
  (assertions/assert-true (= `if 'clojure.core/if))
  (assertions/assert-true (= `-> 'clojure.core/->))
  (assertions/assert-true (= `recur 'recur))
  (assertions/assert-true (= `quote 'quote))
  (assertions/assert-true (= `deftest 'tests.utils.deftest))
  )
