(ns examples.speedtest)

(deftype A [x]
    (strvalue [self] (str x)))

(deftype B [x]
    (strvalue [self] (str (+ x 1))))

(deftype C [x]
    (strvalue [self] (str (dec x))))


(defn getstr [obj]
    (.strvalue obj))

(defn run []
    (time (dotimes [x 10000000]
        (getstr (A. x))
        (getstr (B. x))
        (getstr (C. x)))))
