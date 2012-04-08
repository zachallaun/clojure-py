(ns tests.re-tests
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]])
    (:require [re]))

(deftest re-pattern-tests
    (def PatternType (py/type (re/compile "")))
    (assertions/assert-equal PatternType (py/type (re-pattern "")))
    (assertions/assert-equal PatternType (py/type (re-pattern "foo")))
    (assertions/assert-true (instance?  PatternType (re-pattern "foo")))
    (assertions/assert-true (instance?  PatternType (re-pattern "^(.*)(foo)(bar)+$"))))

(deftest re-matcher-tests
    (def MatchObjectType (py/type (.match (re-pattern "^.*$") "foo")))
    (def testPattern "^(foo).(bar)$")
    (def inputString "foo-bar")
    (def get-matcher (re-matcher (re-pattern testPattern) inputString)) 
    (assertions/assert-not-nil (re-matcher (re-pattern "foo") ""))
    (assertions/assert-true (instance? MatchObjectType (re-matcher (re-pattern "^.*$") "foo")))
    (assertions/assert-true (instance? MatchObjectType get-matcher))
    (assertions/assert-equal "foo-bar" (.group get-matcher 0))
    (assertions/assert-equal "foo" (.group get-matcher 1))
    (assertions/assert-equal "bar" (.group get-matcher 2)))

(deftest re-groups-tests
   (assertions/assert-equal "" (re-groups (re-matcher (re-pattern "") "")))
   (assertions/assert-equal "" (re-groups (re-matcher (re-pattern "") "foo")))
   (assertions/assert-equal "" (re-groups (re-matcher (re-pattern "foo") "")))
   (assertions/assert-equal "bar" (re-groups (re-matcher (re-pattern "foo") "bar")))
   (assertions/assert-equal "foo" (re-groups (re-matcher (re-pattern "foo") "foo")))
   (assertions/assert-equal ["foo" "foo"] (re-groups (re-matcher (re-pattern "(foo)") "foo")))
   (assertions/assert-equal ["foobar" "foo" "bar"] (re-groups (re-matcher (re-pattern "(foo)(bar)") "foobar")))
   (assertions/assert-equal ["foobbbbbarrrbbbaaazzz" "foo" "bbbbbarrr" "bbbaaazzz"] (re-groups (re-matcher (re-pattern "(fo+)(b+.r*)(b*a*z*)") "foobbbbbarrrbbbaaazzz")))
   (assertions/assert-equal ["foobarbaz" "foo" "barbaz" "baz"] (re-groups (re-matcher (re-pattern "(foo)(bar(baz))") "foobarbaz")))
)

