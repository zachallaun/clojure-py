(ns examples.pidigits)
; example taken from "the benchmarking game" 
; http://shootout.alioth.debian.org/u32/program.php?test=pidigits&lang=clojure&id=2

; CPython 2.6 - 1m16sec
; CPython 2.7 - 1m2sec
; PyPy 1.8 - 2m6sec
; Clojure-jvm 1.2 56sec (running original version)


(defmacro bigint [x] x)

(defn printf [f & args]
    (py.bytecode/PRINT_ITEM (py.bytecode/BINARY_MODULO f args)))

(defn flush [] nil)

(defn floor-ev [q r s t x]
  (quot (+ (* q x) r) (+ (* s x) t)))

(defn ncomp [q r s t q2 r2 s2 t2]
  [(+ (* q q2) (* r s2))
   (+ (* q r2) (* r t2))
   (+ (* s q2) (* t s2))
   (+ (* s r2) (* t t2))])

(defn digit [k q r s t n row col]
  (if (> n 0)
    (let [y (floor-ev q r s t 3)]
      (if (== y (floor-ev q r s t 4))
	(let [[q r s t] (ncomp 10 (* -10 y) 0 1 q r s t)]
	  (if (== col 10)
	    (let [row (+ row 10)]
	      (printf "\t:%d\n%d" row y)
	      (recur k q r s t (dec n) row 1))
	    (do (printf "%d" y)
		(recur k q r s t (dec n) row (inc col)))))
	(let [[q r s t] (ncomp q r s t k (* 2 (inc (* 2 k))) 0 (inc (* 2 k)))]
	  (recur (inc k) q r s t n row col))))
    (printf "%s\t:%d\n" (apply str (repeat (- 10 col) " ")) (+ row col))))

(defn -main [& args]
  (let [n (int (first args))]
    (digit 1 (bigint 1) (bigint 0) (bigint 0) (bigint 1) n 0 0))
  (flush))

(-main 10000)
