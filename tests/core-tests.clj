(ns tests.core
    (:require [tests.assertions :as assertions])
    (:require [tests.utils :only [deftest]]))


(deftest if-not-tests
    (assertions/assert-true (if-not false true))
    (assertions/assert-false (if-not true true false))
    (assertions/assert-true (if-not true false true)))

(deftest and-tests
    (assertions/assert-true (and true true true))
    (assertions/assert-false (and true false true))
    (assertions/assert-false (and nil true)))

(deftest identical?-tests
    (assertions/assert-true (identical? 1 1))
    (assertions/assert-false (identical? 1 2))
    (assertions/assert-false (identical? '() '()))
    (let [tmp '()]
         (assertions/assert-true (identical? tmp tmp))))

(deftest compare-tests
    (assert-lesser (compare 0 1) 0)
    (assert-lesser (compare -1 0) 0))

(deftest or-tests
    (assertions/assert-true (or true false false))
    (assertions/assert-false (or false false false))
    (assertions/assert-true (or false true false)))

(deftest zero?-tests
    (assertions/assert-true (zero? 0))
    (assertions/assert-false (zero? 1)))

(deftest count-tests
    (assert-equal (count '()) 0)
    (assert-equal (count '(1)) 1))

(deftest int-tests
    (assert-equal (int "1") 1)
    (assert-equal (int "-1") -1))

(deftest <-tests
    (assertions/assert-true (< 1))
    (assertions/assert-true (< 1 2))
    (assertions/assert-true (< 1 2 3))
    (assertions/assert-true (< 1 2 3 4))
    (assertions/assert-false (< 1 3 2 4)))

(deftest reduce1-tests
    (assert-equal (reduce1 (fn [x y] (py.bytecode/BINARY_ADD x y))
                            '(1 2 3 4)) 10)
    (assert-equal (reduce1 (fn [x y] (py.bytecode/BINARY_ADD x y))
                            5 '(1 2 3 4)) 15))
(deftest reverse-tests
    (assert-equal (reverse [1 2 3 4 5]) [5 4 3 2 1]))

(deftest >1?-tests
    (assertions/assert-true (>1? 3))
    (assertions/assert-false (>1? 0)))

(deftest >0?-tests
    (assertions/assert-true (>0? 2))
    (assertions/assert-false (>0? -1)))


(deftest +-tests
    (assert-equal (+) 0)
    (assert-equal (+ 1) 1)
    (assert-equal (+ 1 2) 3)
    (assert-equal (+ 1 2 3 4) 10))

(deftest *-tests
    (assert-equal (*) 1)
    (assert-equal (* 1) 1)
    (assert-equal (* 1 2) 2)
    (assert-equal (* 1 2 3 4) 24))

(deftest /-tests
    (assert-equal (/ 1) 1)
    (assert-equal (/ 2 2) 1)
    (assert-equal (/ 20 5 2) 2))

(deftest /-tests
    (assert-equal (- 1) -1)
    (assert-equal (- 2 2) 0)
    (assert-equal (- 20 5 2) 13))

(deftest <=-tests
    (assertions/assert-true (<= 1))
    (assertions/assert-true (<= 1 2))
    (assertions/assert-true (<= 1 1 3 4))
    (assertions/assert-false (<= 2 1 1 1)))

(deftest >-tests
    (assertions/assert-true (> 4))
    (assertions/assert-true (> 4 3))
    (assertions/assert-true (> 4 3 2 1))
    (assertions/assert-false (> 4 3 2 2)))

(deftest >=-tests
    (assertions/assert-true (>= 4))
    (assertions/assert-true (>= 4 3))
    (assertions/assert-true (>= 4 3 3))
    (assertions/assert-false (>= 3 4 2 1)))

(deftest ==-tests
    (assertions/assert-true (== 1))
    (assertions/assert-true (== 1 1))
    (assertions/assert-true (== 1 1 1 1 1))
    (assertions/assert-false (== 1 2 1 1 1)))

(deftest max-tests
    (assert-equal (max 1) 1)
    (assert-equal (max 1 2) 2)
    (assert-equal (max 3 2 1) 3))

(deftest min-tests
    (assert-equal (min 1) 1)
    (assert-equal (min 1 2) 1)
    (assert-equal (min 3 2 1) 1))

(deftest pos?-tests
    (assertions/assert-true (pos? 1))
    (assertions/assert-false (pos? -1))
    (assertions/assert-false (pos? 0)))

(deftest neg?-tests
    (assertions/assert-true (neg? -1))
    (assertions/assert-false (neg? 1))
    (assertions/assert-false (neg? 0)))

(deftest quot-tests
    (assert-equal (quot 23 7) 3)
    (assert-equal (quot 4 2) 2)
    (assert-equal (quot 3 2) 1)
    (assert-equal (quot 6 4) 1)
    (assert-equal (quot 0 5) 0)
    
    ;(assert-equal (quot 2 1/2) 4)
    ;(assert-equal (quot 2/3 1/2) 1)
    ;(assert-equal (quot 1 2/3) 1)
    
    (assert-equal (quot 4.0 2.0) 2.0)
    (assert-equal (quot 4.5 2.0) 2.0)
    ; |num| > |div|, num != k * div
    (assert-equal (quot 42 5) 8)     ; (8 * 5) + 2 == 42
    (assert-equal (quot 42 -5) -8)   ; (-8 * -5) + 2 == 42
    (assert-equal (quot -42 5) -8)   ; (-8 * 5) + -2 == -42
    (assert-equal (quot -42 -5) 8)   ; (8 * -5) + -2 == -42
    ; |num| > |div|, num = k * div
    (assert-equal (quot 9 3) 3)
    (assert-equal (quot 9 -3) -3)
    (assert-equal (quot -9 3) -3)
    (assert-equal (quot -9 -3) 3)
    ; |num| < |div|
    (assert-equal (quot 2 5) 0)
    (assert-equal (quot 2 -5) 0)
    (assert-equal (quot -2 5) 0)
    (assert-equal (quot -2 -5) 0)

    ; num = 0, div != 0
    (assert-equal (quot 0 3) 0)
    (assert-equal (quot 0 -3) 0)
    )
    

(deftest rem-tests
    (assert-equal (rem 23 7) 2)
    (assert-equal (rem 4 2) 0)
    (assert-equal (rem 3 2) 1)
    (assert-equal (rem 6 4) 2)
    (assert-equal (rem 0 5) 0)

    ;(assert-equal (rem 2 1/2) 0)
    ;(assert-equal (rem 2/3 1/2) 1/6)
    ;(assert-equal (rem 1 2/3) 1/3)

    (assert-equal (rem 4.0 2.0) 0.0)
    (assert-equal (rem 4.5 2.0) 0.5)

    ; |num| > |div|, num != k * div
    (assert-equal (rem 42 5) 2)      ; (8 * 5) + 2 == 42
    (assert-equal (rem 42 -5) 2)     ; (-8 * -5) + 2 == 42
    (assert-equal (rem -42 5) -2)    ; (-8 * 5) + -2 == -42
    (assert-equal (rem -42 -5) -2)   ; (8 * -5) + -2 == -42

    ; |num| > |div|, num = k * div
    (assert-equal (rem 9 3) 0)
    (assert-equal (rem 9 -3) 0)
    (assert-equal (rem -9 3) 0)
    (assert-equal (rem -9 -3) 0)

    )

(deftest mod-tests
    (assert-equal (rem 23 7) 2)
    (assert-equal (mod 4 2) 0)
    (assert-equal (mod 3 2) 1)
    (assert-equal (mod 6 4) 2)
    (assert-equal (mod 0 5) 0)

    ;(assert-equal (mod 2 1/2) 0)
    ;(assert-equal (mod 2/3 1/2) 1/6)
    ;(assert-equal (mod 1 2/3) 1/3)

    (assert-equal (mod 4.0 2.0) 0.0)
    (assert-equal (mod 4.5 2.0) 0.5)

    ; |num| > |div|, num != k * div
    (assert-equal (mod 42 5) 2)      ; (42 / 5) * 5 + (42 mod 5)        = 8 * 5 + 2        = 42
    (assert-equal (mod 42 -5) -3)    ; (42 / -5) * (-5) + (42 mod -5)   = -9 * (-5) + (-3) = 42
    (assert-equal (mod -42 5) 3)     ; (-42 / 5) * 5 + (-42 mod 5)      = -9 * 5 + 3       = -42
    (assert-equal (mod -42 -5) -2)  ; (-42 / -5) * (-5) + (-42 mod -5) = 8 * (-5) + (-2)  = -42

    ; |num| > |div|, num = k * div
    (assert-equal (mod 9 3) 0)      ; (9 / 3) * 3 + (9 mod 3) = 3 * 3 + 0 = 9
    (assert-equal (mod 9 -3) 0)
    (assert-equal (mod -9 3) 0)
    (assert-equal (mod -9 -3) 0)

    ; |num| < |div|
    (assert-equal (mod 2 5) 2)       ; (2 / 5) * 5 + (2 mod 5)        = 0 * 5 + 2          = 2
    (assert-equal (mod 2 -5) -3)     ; (2 / -5) * (-5) + (2 mod -5)   = (-1) * (-5) + (-3) = 2
    (assert-equal (mod -2 5) 3)      ; (-2 / 5) * 5 + (-2 mod 5)      = (-1) * 5 + 3       = -2
    (assert-equal (mod -2 -5) -2)    ; (-2 / -5) * (-5) + (-2 mod -5) = 0 * (-5) + (-2)    = -2

    ; num = 0, div != 0
    (assert-equal (mod 0 3) 0)       ; (0 / 3) * 3 + (0 mod 3) = 0 * 3 + 0 = 0
    (assert-equal (mod 0 -3) 0)

    ; large args
    (assert-equal (mod 3216478362187432 432143214) 120355456)
)


(deftest bit-not-tests
    (assert-equal (bit-not 5) -6))

(deftest bit-and-tests
    (assert-equal (bit-and 5 4) 4)
    (assert-equal (bit-and 5 4 1) 0))

(deftest bit-or-tests
    (assert-equal (bit-or 6 5 4 2) 7))

(deftest bit-xor-tests
    (assert-equal (bit-xor 2 3 4) 5))

(deftest bit-and-not-tests
    (assert-equal (bit-and-not 3 1 2) 0))

(deftest bit-shift-left-tests
    (assert-equal (bit-shift-left 1 3) 8))

(deftest bit-shift-right-tests
    (assert-equal (bit-shift-right 8 3) 1))

(deftest bit-clear-tests
    (assert-equal (bit-clear 3 1) 1))

(deftest bit-set-tests
    (assert-equal (bit-set 0 1) 2))

(deftest bit-flip-tests
    (assert-equal (bit-flip 0 1) 2)
    (assert-equal (bit-flip 2 1) 0))

(deftest bit-flip-tests
    (assertions/assert-true (bit-test 3 1))
    (assertions/assert-false (bit-test 1 1)))

(deftest integer?-tests
    (assertions/assert-true (integer? 1))
    (assertions/assert-false (integer? "1")))

(deftest even?-tests
    (assertions/assert-true (even? 2))
    (assertions/assert-false (even? 1)))

(deftest odd?-tests
    (assertions/assert-true (odd? 1))
    (assertions/assert-false (odd? 2)))

(deftest complement-tests
    (assertions/assert-true ((complement (fn [] false))))
    (assertions/assert-true ((complement (fn [x] false)) 1))
    (assertions/assert-true ((complement (fn [x y] false)) 1 2))
    (assertions/assert-true ((complement (fn [x y z] false)) 1 2 3)))

(deftest constantly-tests
    (assert-equal ((constantly 1) 1 2 3 4 5) 1))

(deftest identityi-tests
    (assert-equal (identity 3) 3)
    (assert-equal (identity 4) 4))

(deftest peek-tests
    (assert-equal (peek '(1 2)) 1)
    (assert-equal (peek nil) nil))

(deftest pop-tests
    (assert-equal (pop '(1 2)) '(2))
    (assert-equal (pop nil) nil))

;;map stuff

(deftest contains?-tests
    (assertions/assert-true (contains? [4 4 4 4] 3))
    (assertions/assert-true (contains? {:a 1 :b 2} :a))
    (assertions/assert-false (contains? [1 1 1] 4))
    (assertions/assert-false (contains? {:a 4} :b)))

(deftest get-tests
    (assert-equal (get {:a 1} :a) 1)
    (assert-equal (get "abc" 1) "b"))

(deftest dissoc-tests
    (assert-equal (dissoc {:a 1 :b 2} :b) {:a 1})
    (assert-equal (dissoc {:a 1 :b 2} :a :b) {}))

(deftest find-tests
    (assert-equal (.getKey (find {:a 1} :a)) :a)
    (assert-equal (.getValue (find {:a 1} :a)) 1))

(deftest select-keys-tests
    (assert-equal (select-keys {:a 1 :b 2 :c 3} [:a]) {:a 1}))

(deftest keys-tests
    (assert-equal (keys {:a 1}) [:a]))

(deftest vals-tests
    (assert-equal (vals {:a 1}) [1]))

(deftest key-tests
    (assert-equal (key (find {:a 1 :b 2} :b)) :b))

(deftest val-tests
    (assert-equal (val (find {:a 1 :b 2} :b)) 2))

(deftest name-tests
    (assert-equal (name 'Foo) "Foo")
    (assert-equal (name "Foo") "Foo"))

(deftest namespace-tests
    (assert-equal (namespace 'baz/Foo) "baz")
    (assert-equal (namespace 'Foo) nil))

; broken need to fix
;(deftest dot-dot-tests
;    (assert-equal (.. :foo (.-sym) (.-name)) ":foo"))

(deftest ->-tests
    (assert-equal (-> " baz " (.rstrip) (.lstrip)) "baz"))

;(deftest ->>-tests ; haven't a clue how to test this
;    )


;;; var stuff


;;; if-let and when-let tests are from
;;; http://blog.jayfields.com/2011/03/clojure-if-let-and-when-let.html

(deftest if-let-tests
	(assert-equal (if-let [a 4] (+ a 4) (+ 10 10)) 8)
	(assert-equal (if-let [a nil] (+ a 4) (+ 10 10)) 20))


(deftest when-let
    (assert-equal (when-let [a 9] (+ a 4))  13)
    (assert-equal (when-let [a nil] (+ a 4)) nil))

;;; functional stuff

(deftest comp-tests
    (assert-equal ((comp str +) 8 8 8) "24"))

(deftest juxt-tests
    (assert-equal ((juxt :a :b :c :d) {:a 1 :b 2 :c 3 :d 4}) [1 2 3 4]))

(deftest partial-tests
    (assert-equal ((partial + 1) 1) 2))

;;; sequence stuff

(deftest sequence-tests
    (assert-equal (sequence [1 2 3]) '(1 2 3)))

(deftest every?-tests
    (assertions/assert-true (every? even? '(2 4 6)))
    (assertions/assert-false (every? even? '(1 4 6))))

(deftest every?-tests
    (assertions/assert-false (not-every? even? '(2 4 6)))
    (assertions/assert-true (not-every? even? '(1 4 6))))

(deftest some-tests
    (assertions/assert-true (some even? '(1 2 3 4)))
    (assert-equal (some even? '(1 3 5 7)) nil))

(deftest not-any?-tests
    (assertions/assert-true (not-any? odd? '(2 4 6)))
    (assertions/assert-false (not-any? odd? '(1 2 3))))

;(deftest dotimes-tests
;    (dotimes [n 5] (assertions/assert-true (and (>= n 0) (< n 5)))))

(deftest map-tests
    (assert-equal (map inc [1 2 3 4 5]) (seq [2 3 4 5 6])))

(deftest mapcat-tests
    (assert-equal (mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]]) [0 1 2 3 4 5 6 7 8 9]))
 
(deftest filter-tests
    (assert-equal (filter even? [1 2 3 4 5]) [2 4]))

(deftest remove-tests
    (assert-equal (remove even? [1 2 3 4 5]) [1 3 5]))

(deftest take-tests
    (assert-equal (take 2 [1 2 3 4]) [1 2]))

(deftest take-while-tests
    (assert-equal (take-while even? [2 2 1 1]) [2 2]))

(deftest drop-tests
    (assert-equal (drop 1 [1 2 3]) [2 3]))

(deftest drop-last-tests
    (assert-equal (drop-last 2 [1 2 3 4]) [1 2]))

(deftest take-last-tests
    (assert-equal (take-last 3 [1 2 3 4]) [2 3 4]))

(deftest drop-while-tests
    (assert-equal (drop-while even? [2 4 6 1 2 3]) [1 2 3]))

(deftest cycle-tests
    (assert-equal (take 6 (cycle [1 2 3])) [1 2 3 1 2 3]))

(deftest split-at-tests
    (assert-equal (split-at 3 [1 2 3 4 5]) [[1 2 3] [4 5]]))

(deftest split-with-tests
    (assert-equal (split-with odd? [1 1 1 1 2 2 2 2]) [[1 1 1 1] [2 2 2 2]]))

(deftest repeat-tests
    (assert-equal (repeat 3 1) [1 1 1]))

(deftest interate-tests
    (assert-equal (take 3 (iterate inc 0)) [0 1 2]))

(deftest range-tests
    (assert-equal (range 0 8 2) [0 2 4 6]))

(deftest merge-tests
    (assert-equal (merge {:a 1 :b 2} {:a 3 :c 4}) {:a 3 :b 2 :c 4}))

(deftest merge-with-tests
    (assert-equal (merge-with + 
                   {:a 1  :b 2}
                   {:a 9  :b 98 :c 0})
                  {:c 0, :a 10, :b 100}))


(deftest zipmap-tests
    (assert-equal (zipmap [:a :b :c :d :e] [1 2 3 4 5])
                    {:e 5, :d 4, :c 3, :b 2, :a 1}))


(deftest sort-tests
    (assert-equal (sort [3 1 2 4]) [1 2 3 4])
    (assert-equal (sort > (vals {:foo 5, :bar 2, :baz 10})) [10 5 2]))

(deftest sort-by-tests
    (assert-equal (sort-by first > [[1 2] [2 2] [2 3]]) [[2 2] [2 3] [1 2]]))

(deftype Accum [i]
    (inc [self] (py/setattr self "i" (inc i))))


(deftest dorun-tests
    (let [accum (Accum 0)]
         (dorun (map (fn [x] (.inc accum))
                     (range 10)))
         (assert-equal (.-i accum) 10)))

(deftest nthnext-tests
    (assert-equal (nthnext (range 10) 3) '(3 4 5 6 7 8 9)))

(deftest nth-tests
    (assert-equal (nth (list 1 2 3) 1) 2)
    (assert-equal (nth [1 2 3] 1) 2))

(deftest partition-tests
    (assert-equal (partition 4 (range 20)) 
                  '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (16 17 18 19)))
    (assert-equal (partition 4 6 ["a" "b" "c" "d"] (range 20))
                  '((0 1 2 3) (6 7 8 9) (12 13 14 15) (18 19 "a" "b"))))

(deftest eval-tests
    (assert-equal (eval '(+ 1 2)) 3))

(deftest doseq-tests
    (doseq [x [1 2 3]
                          y [1 2 3]]
                         (py/print (* x y))))
;; prints      
;;[1 2 3 2 4 6 3 6 9]

(deftest do-times
    (let [accum (Accum 0)]
         (dotimes [i 5]
             (assert-equal (.-i accum) i)
             (.inc accum))))

(deftest class-tests
    (assert-equal (class "f") py/str))

(deftest num-tests
    (assert-equal (num "1") 1))

(deftest num-tests
    (assertions/assert-true (float? (num "inf"))))

(deftest number?-tests
    (assertions/assert-true (number? 1)))

(deftest read-string-tests
    (assert-equal (read-string "12") 12))

(deftest subvec-tests
    (assert-equal (subvec [1 2 3 4] 1 3) [2 3]))

(deftest doto-tests
    (assert-equal (doto (py/list) (.append "foo") (.append "bar")) ["foo" "bar"]))

(deftest memfn-tests
    (assert-equal (let [f (memfn join ch)]
                       (f "," ["1" "2"])) 
                  "1,2"))
(deftest set-tests
    (set [1 2 3 4 5]))

(deftest find-ns-tests
    (assertions/assert-true (not (nil? (find-ns 'clojure.core)))))

(deftest create-ns-tests
    (assertions/assert-true (identical? (find-ns 'clojure.core) (create-ns 'clojure.core)))
    (assertions/assert-true (not (nil? (create-ns 'foo.bar)))))

(deftest ns-name-tests
    (assert-equal (ns-name 'clojure.core) 'clojure.core))

(deftest let-tests
    (let [[x & y] [1 2 3]]
         (assert-equal x 1)
         (assert-equal y [2 3])))

(deftest let-tests
    ((fn td [[x & y]]
         (assert-equal x 1)
         (assert-equal y [2 3])) [ 1 2 3]))

(deftest loop-tests
    (loop [[x & y] [1 2 3]]
         (assert-equal x 1)
         (assert-equal y [2 3])))

(deftest when-first-tests
    (assert-equal (when-first [a [1 2 3]] a) 1)
    (assert-equal (when-first [a []] a) nil)
    (assert-equal (when-first [a nil] a) nil))

(deftest lazy-cat-tests
    (assert-equal (lazy-cat [1 2 3] [4 5 6]) [1 2 3 4 5 6]))

(deftest for-tests
    (assert-equal (for [x [1 2 3]] x) [1 2 3]))

(deftest destructure-tests
    (assert-equal (map (fn [[k v]] k) {:1 1 :2 2}) [:1 :2])
    (assert-equal (map (fn [[k v]] v) {:1 1 :2 2}) [1 2]))

(deftest map-entry-tests
    (assert-equal (-> {:1 :2} first first) :1)
    (assert-equal (-> {:1 :2} first second) :2))
    

(deftest reduce-tests
    (assert-equal (reduce + '(1 2 3 4)) 10)
    (assert-equal (reduce + 5 '(1 2 3 4)) 15))

(deftest empty?-tests
    (assertions/assert-true (empty? []))
    (assertions/assert-false (empty? [1])))



(deftest do-tests
    (assert-equal (do) nil)
    (assert-equal (do 1) 1)
    (assert-equal (do 1 2) 2))

(deftest lazy-seq-tests
    (.more (range 1))) ; would throw an error before fix to Issue #45

(deftest comment-tests
    (comment (assert-true false)))

(py/print "all tests passed")
