;;; persistentvector-tests.clj
;;;
;;; Sunday, March 25 2012

(ns tests.persistentvector-tests
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]]))

(deftest creation-tests
  ;; XXX: AbstractMethodCall: in ChunkedCons
  ;;      ChunkedCons.first() not found, calls ISeq.first()
  ;; (assertions/assert-equal (vec (range 3)) [0 1 2])
  )

(deftest call-index-tests
  (assertions/assert-equal ([0] 0) 0)
  (assertions/assert-equal ([0 1] 1) 1)
  (assertions/assert-equal ([0 1 2] 2) 2)
  
  ;; AttributeError: type object 'PersistentVector' has not attribuet 'Node'
  ;; 
  ;; (assertions/assert-equal ([1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  ;;                            1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  ;;                            1]         ; fails when |v| > 32
  ;;                             5)
  ;;                          1)
  )


