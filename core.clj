(ns cs441.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:use clojure.test))

;;------------------------------------------------------------------------------

;;Implemented merge sort algorithm by Alexei Sholik: https://gist.github.com/alco/2135276

(defn merge-lists
  "Merges two sorted sequences into a single sorted sequence"
  ([left right]
   (merge-lists (list left right)))
  ([[left right]]
   (loop [l left, r right, result []]
     (let [lhead (first l), rhead (first r)]
       (cond
         (nil? lhead)     (concat result r)
         (nil? rhead)     (concat result l)
         (<= lhead rhead) (recur (rest l) r (conj result lhead))
         true             (recur l (rest r) (conj result rhead)))))))

(defn naive-merge-sort
  "Produces a sorted sequence from an input sequence.
  Works best with vectors (since it uses 'count' internally)."
  [xs]
  ((fn mergesort-counted [xs n]
     (if (<= n 1)
       xs
       (let [middle (bit-shift-right n 1)]  ; fast division by 2
         (merge-lists (map mergesort-counted 
                        (split-at middle xs)        ; two halves
                        [middle (- n middle)])))))  ; count of each half
   xs (count xs)))

;;-----------------------------------------------------------------------------

;;Parallelized merge sort algorithm by Yuri Borodin

(defn parallel-merge-sort-2 [list]
  (if (< (count list) 2) list
    (apply merge-lists
      (pmap naive-merge-sort
        (split-at (/ (count list) 2) list)))))

(defn parallel-merge-sort-4 [list]
  (if (< (count list) 2) list
    (apply merge-lists
      (pmap parallel-merge-sort-2
        (split-at (/ (count list) 2) list)))))

(defn parallel-merge-sort-8 [list]
  (if (< (count list) 2) list
    (apply merge-lists
      (pmap parallel-merge-sort-4
        (split-at (/ (count list) 2) list)))))

(defn parallel-merge-sort-16 [list]
  (if (< (count list) 2) list
    (apply merge-lists
      (pmap parallel-merge-sort-8
        (split-at (/ (count list) 2) list)))))

(defn parallel-merge-sort-32 [list]
  (if (< (count list) 2) list
    (apply merge-lists
      (pmap parallel-merge-sort-16
        (split-at (/ (count list) 2) list)))))

;;-----------------------------------------------------------------------------

;;List evaluation function for debugging by user Fabricator: https://goo.gl/vqpHE5
(defn list-eval
  [x]
  (if (list? x)
    (for [lst x] (list-eval lst))
    (if (integer? x)
      (println "")
      (println "This list contains a non-numeric value"))))

;;------------------------------------------------------------------------------

;;Function for parsing strings read from file into ints

(defn parse-int [s]
   (.parse (java.text.NumberFormat/getInstance) s))

;;------------------------------------------------------------------------------
;; Function for reading lines from file
;; The parse-int function is mapped to each line that is read in
;; Produces a list of ints

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (map parse-int(line-seq r)))))

;;------------------------------------------------------------------------------

;; Custom macro that times a function's runtime by user DarryIG: https://goo.gl/BPT7W2
(defmacro bench
  " Times the execution of your function,
    discarding the output and returning the elapsed time in seconds
    (you can modify this by changing the divisor from 1e9 (i.e. for milliseconds it would be 1e6."
  ([& forms]
   `(let [start# (System/nanoTime)]
      ~@forms
      (double (/ (- (System/nanoTime) start#) 1e9)))))   ; Time in seconds

;;-----------------------------------------------------------------------------

;; Main

(defn -main[]
  
  (def lines (get-lines "numbers.txt")) 
  
  ;;For Debugging
  ;(println (type lines))
  ;(def f (first lines))
  ;(println (integer? f))
  ;(println (list? lines)))
  ;(run! println lines))
  
  ;;Timing Merge Sort
  (dotimes [n 5] (println n "Single-Thread Merge Sort took" (bench(naive-merge-sort lines)) "seconds"))
  (dotimes [n 5](println n "Parallel Merge Sort with 2 threads took" (bench(parallel-merge-sort-2 lines)) "seconds"))
  (dotimes [n 5] (println n "Parallel Merge Sort with 4 threads took" (bench(parallel-merge-sort-4 lines)) "seconds"))
  (dotimes [n 5] (println n "Parallel Merge Sort with 8 threads took" (bench(parallel-merge-sort-8 lines)) "seconds"))
  (dotimes [n 5] (println n "Parallel Merge Sort with 16 threads took" (bench(parallel-merge-sort-16 lines)) "seconds"))
  (dotimes [n 5] (println n "Parallel Merge Sort with 32 threads took" (bench(parallel-merge-sort-32 lines)) "seconds"))
  (System/exit 0)) ;;Forces the system to close

 
;; For Debugging (Hard Coded List)
  ;(def myList '(2, 8, 5, 3)))
  ;(println (type myList))
  ;(def f1 (first myList))
  ;(println (integer? f1))
  ;(println (mrgsrt myList)))
  ;(run! println myList))
(run-tests)

  

  

  
