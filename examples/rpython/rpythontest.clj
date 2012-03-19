(ns rpythontest)

(defn run_x [x]
  (when (< x 100)
        (py.bytecode/PRINT_ITEM x)
        (recur (inc x))))


; example taken from "the benchmarking game" 
; http://shootout.alioth.debian.org/u32/program.php?test=pidigits&lang=clojure&id=2

; CPython 2.6 - 1m16sec
; CPython 2.7 - 1m2sec
; PyPy 1.8 - 2m6sec
; Clojure-jvm 1.2 56sec (running original version)


(defn fact {:static true} [x]
    (loop [n x f 1]
        (if (= n 1)
            f
            (recur (dec n) (* f n)))))

(defn test {:static true} [times]
    (loop [rem times]
        (py.bytecode/PRINT_ITEM rem)
        (if (> rem 0)
            (do (fact 20)
                (recur (dec rem))))))

;; on my machine
;; clojure jvm : 25 sec
;; clojure pypy : 48 sec
;; clojure on rpython: 6sec

(defn main {:static true} [x]
     (py.bytecode/PRINT_ITEM (test 19999999)))




