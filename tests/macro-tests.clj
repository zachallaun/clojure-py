(ns tests.macro-tests
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]]))

; This test depends on macro definitions in clojure.core,
; which might change in the future.
(deftest core-expansion-tests
  (assertions/assert-equal (macroexpand-1 '(when a b))
                           '(clojure.core/if a (do b)))
  (assertions/assert-equal (macroexpand '(when a b))
                           '(if* a (do b))))

(defmacro foo1 [& body] (cons 'do body))
(defmacro foo2 [& body] (list 'foo1 (cons 'foo1 body)))

; A brute-force test in order to create some failure
; if there's a bug in macroexpand-1. For now, macroexpand
; doesn't work correctly inside tests because *ns* is not
; set correctly.
(assert (= (macroexpand-1 '(foo1 a b c)) '(do a b c)))
(assert (= (macroexpand-1 '(foo2 a b c)) '(foo1 (foo1 a b c))))
(assert (= (macroexpand '(foo2 a b c)) '(do (foo1 a b c))))

(comment
  ; This is how it should be done.
  (deftest expansion-tests
    (assertions/assert-equal (macroexpand-1 '(foo1 a b c)) '(do a b c))
    (assertions/assert-equal (macroexpand-1 '(foo2 a b c)) '(foo1 (foo1 a b c)))
    (assertions/assert-equal (macroexpand '(foo2 a b c)) '(do (foo1 a b c)))))

