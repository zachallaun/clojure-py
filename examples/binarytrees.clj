;; The Computer Language Benchmarks Game

;; http://shootout.alioth.debian.org/

;
;;; Adapted from the Clojure-jvm version
;; Adapted from the Java -server version

;

;; contributed by Marko Kocic

;; modified by Kenneth Jonsson, restructured to allow usage of 'pmap'

;; modified by Andy Fingerhut to use faster primitive math ops, and

;; deftype instead of defrecord for smaller tree nodes.
;; modified by Mike Anderson to avoid boxing overheads


(ns binarytrees)


;; These TreeNode's take up noticeably less memory than a similar one

;; implemented using defrecord.

(defn println [s] (py/print s))
(defn format [s & args] (py.bytecode/BINARY_MODULO s args))

(deftype TreeNode [left right item])

(defn bottom-up-tree [item depth]
  (let [int-item (int item)
        int-depth (int depth)]
    (if (<= depth 0)
      (TreeNode. nil nil int-item)
      (TreeNode.
       (bottom-up-tree (dec (* (int 2) int-item))
                       (dec int-depth))
       (bottom-up-tree (* (int 2) int-item)
                       (dec int-depth))
       int-item))))

(defn item-check [node]
  (let [item (int (.-item node))]
	  (if-not (.-left node)
	    item
	    (+ 
         (+ 
           item 
           (int (item-check (.-left node))))
	       (- 
           (int (item-check (.-right node))))))))

    
(defn ^:static check-trees [i acc d]    
  (if (<= i 0)
    acc
    (let [value (+ 
                  (int (item-check (bottom-up-tree i d)))
                  (int (item-check (bottom-up-tree (- i) d))))]
      (recur (dec i) (+ acc value) d))))

(defn iterate-trees 
  ([mx mn d]
    (let [iterations (bit-shift-left 1 (int (+ mx mn (- d))))]
      (format "%d\t trees of depth %d\t check: %d" (* 2 iterations) d (check-trees iterations 0 d)))))

(def min-depth 4)

(defn main [max-depth]
  (let [stretch-depth (int (inc max-depth))]
    (let [tree (bottom-up-tree 0 stretch-depth)
          check (item-check tree)]
      (println (format "stretch tree of depth %d\t check: %d" stretch-depth check)))
    (let [int-lived-tree (bottom-up-tree 0 max-depth) ]
      ;; The following line is where Kenneth Jonsson used pmap.  On a

      ;; 1-core machine, I have found significantly less user+system

      ;; CPU time used when it is map, and slightly less elapsed time

      ;; (at the cost of more user+system CPU time) when it is pmap.

      (doseq [trees-nfo (map (fn [d]
                                (iterate-trees max-depth min-depth d))
			      (range min-depth stretch-depth 2)) ]
        (println trees-nfo))
      (println (format "long lived tree of depth %d\t check: %d" max-depth (item-check int-lived-tree))))))

(defn -main [& args]
  (let [n (if (first args) (int (first args)) 0)
        max-depth (if (> (+ min-depth 2) n) (+ min-depth 2) n)]
    (main max-depth)))

(-main 12)
