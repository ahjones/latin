(ns latin.core
  (:use [clojure.contrib.seq-utils :only [positions]]))

(def sqrt (memoize (fn [x] (Math/sqrt x))))

(def alphabet (memoize (fn [sq] (range 1 (sqrt (inc (count sq)))))))

(defn rows [sq] (partition (sqrt (count sq)) sq))

(defn cols [sq] (rows (apply interleave (rows sq))))

(defn valid-line [line] (or
			 (every? #(= :unknown %) line)
			 (apply distinct? (filter #(not= :unknown %) line))))

(def rules
    [#(every? valid-line (rows %))
     #(every? valid-line (cols %))])

(defn valid-square? [sq] (every? #(% sq) rules))
(defn finished? [sq] (= 0 (count (filter #(= :unknown %) sq))))

(defn next-sq [sq]
  (for [x (alphabet sq)
	:let [n (assoc sq (first (positions #(= :unknown %) sq)) x)]
	:when (valid-square? n)] n))

(defn latin-tree [sq] (tree-seq (complement finished?) next-sq sq))
(defn latin [sq]
  (for [sq (latin-tree sq) :when (finished? sq)] sq))

(def eg [:unknown :unknown :unknown :unknown :unknown :unknown :unknown :unknown :unknown])

(latin eg)
