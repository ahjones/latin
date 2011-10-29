(ns latin.core
  (:use [clojure.contrib.seq-utils :only [positions]]))

(def sqrt (memoize (fn [x] (Math/sqrt x))))

(def alphabet (memoize (fn [sq] (range 1 (sqrt (inc (count sq)))))))

(defn rows
 "A sequence of the rows of the square"
 [sq]
 (partition (sqrt (count sq)) sq))

(defn cols
 "A sequence of the columns of the square"
 [sq]
 (rows (apply interleave (rows sq))))

(defn valid-line [line] (or
			 (every? #(= :unknown %) line)
			 (apply distinct? (filter #(not= :unknown %) line))))

(def rules
    [#(every? valid-line (rows %))
     #(every? valid-line (cols %))])

(defn valid-square? [sq] (every? #(% sq) rules))

(defn finished? [sq] (= 0 (count (filter #(= :unknown %) sq))))

(defn next-sq [sq] (for [x (alphabet sq)] (assoc sq (first (positions #(= :unknown %) sq)) x)))

(defn latin2 [sols sq]
  ;; (println (str sq " : " sols))
  (if (valid-square? sq)
    (if (finished? sq)
      (cons sq sols)
      (reduce latin2 sols (next-sq sq)))
    sols))

(defn latin [sq] (latin2 () sq))

(def eg [1 2 3 :unknown :unknown :unknown :unknown :unknown :unknown])

(latin eg) ;; ([1 2 3 3 1 2 2 3 1] [1 2 3 2 3 1 3 1 2])