(ns tests.macro-tests
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]]))

; This test depends on macro definitions in clojure.core,
; which might change in the future.
(deftest core-expansion-tests
   (assertions/assert-equal (macroexpand-1 '(when a b)) '(if a (do b)))
   (assertions/assert-equal (macroexpand '(when a b)) '(if* a (do b))))

(comment
  ; This test doesn't work yet because macroexpand doesn't have
  ; access to the namespace in which it was called at compilation time.
  (defmacro foo1 [& body] `(do ~@body))
  (defmacro foo2 [& body] `(foo1 (foo1 ~@body)))
  (deftest expansion-tests
    (assertions/assert-equal (macroexpand-1 '(foo1 a b c)) '(do a b c))
    (assertions/assert-equal (macroexpand-1 '(foo2 a b c)) '(foo1 (foo1 a b c)))
    (assertions/assert-equal (macroexpand '(foo2 a b c)) '(do (foo1 a b c)))))

