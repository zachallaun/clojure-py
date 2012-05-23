(ns tests.persistenttreemap-tests
    (:use [clojure.lang.persistenttreemap :only [PersistentTreeMap]])
    (:require [tests.assertions :as assertions])
    (:use [tests.utils :only [deftest]]))

(def testmap (-> (PersistentTreeMap)
                 (.assoc "a" 1)
                 (.assoc "b" 2)))

(deftest assoc-tests
    (assertions/assert-true (.containsKey testmap "a")))

(deftest without-tests
    (assertions/assert-false (-> testmap
                                 (.without "a")
                                 (.containsKey "a"))))

(deftest seq-tests
    (assertions/assert-equal (.seq (PersistentTreeMap)) py/None)
    (assertions/assert-equal (-> testmap .seq .first .key) "a")
    (assertions/assert-equal (-> testmap .seq .next .first .key) "b"))

(deftest keys-tests
    (let [ks (.keys testmap)]
        (assertions/assert-equal (.next ks) "a")
        (assertions/assert-equal (.next ks) "b")))

(deftest vals-tests
    (let [vs (.vals testmap)]
        (assertions/assert-equal (.next vs) 1)
        (assertions/assert-equal (.next vs) 2)))

(deftest minkey-tests
    (assertions/assert-equal (.minKey testmap) "a"))

(deftest minkey-tests
    (assertions/assert-equal (.maxKey testmap) "b"))

(deftest valat-tests
    (assertions/assert-equal (.valAt testmap "a") 1))
