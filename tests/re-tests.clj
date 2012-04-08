(ns tests.re-tests
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]])
    (:require [re]))

(def PatternType (py/type (re/compile "")))

(deftest re-pattern-tests
    (assertions/assert-equal PatternType (py/type (re-pattern "foo")))
    (assertions/assert-true (instance?  PatternType (re-pattern "foo")))
    (assertions/assert-true (instance?  PatternType (re-pattern "^(.*)(foo)(bar)+$"))))

(def MatchObjectType (py/type (.match (re-pattern "^.*$") "foo")))

(deftest re-matcher-tests
    (def testPattern "^(foo).(bar)$")
    (def inputString "foo-bar")
    (def get-matcher (re-matcher (re-pattern testPattern) inputString)) 
    (assertions/assert-true (instance? MatchObjectType (re-matcher (re-pattern "^.*$") "foo")))
    (assertions/assert-true (instance? MatchObjectType get-matcher))
    (assertions/assert-equal "foo-bar" (.group get-matcher 0))
    (assertions/assert-equal "foo" (.group get-matcher 1))
    (assertions/assert-equal "bar" (.group get-matcher 2)))

;(deftest re-groups-tests
;   (assertions/assert-equal "foo" (re-groups (re-pattern "foo" "foo"))))

