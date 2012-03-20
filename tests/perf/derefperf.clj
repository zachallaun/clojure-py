(ns tests.perf.derefperf)

(require 'time)

(deftype V [i]
    (deref [self] i))


(deftype V2 [i]
    (deref [self] 1))

(def v (V 1))
(def v2 (V2 1))


(def t1 (time/time))
(dotimes [x 10000]
    (dotimes [y 10000]
        (let [z (+ x y)]
            (+ z (.deref v)))))
(def t2 (time/time))
(println (str "Deref test 1 completed in " (- t2 t1) " seconds."))

(def t1 (time/time))
(dotimes [x 10000]
    (dotimes [y 10000]
        (let [z (+ x y)]
                (+ z (.deref v2)))))
(def t2 (time/time))
(println (str "Deref test 2 completed in " (- t2 t1) " seconds."))

(def t1 (time/time))
(dotimes [x 10000]
    (dotimes [y 10000]
        (let [z (+ x y)]
             (+ z 1))))
(def t2 (time/time))
(println (str "No-deref test completed in " (- t2 t1) " seconds."))
