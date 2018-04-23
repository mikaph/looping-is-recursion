(ns looping-is-recursion)


(defn
  power
  [base exp]
  (let [helper (fn [acc n]
                 (cond
                   (= n 1) acc
                   (zero? n) 1
                   :else (recur (* acc base) (dec n))))]
    (helper base exp)))


(defn
  last-element
  [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn
  seq=
  [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))


(defn
  find-first-index
  [pred a-seq]
  (loop [index 0
         r-seq a-seq]
    (cond
      (empty? r-seq) nil
      (pred (first r-seq)) index
      :else (recur (inc index) (rest r-seq)))))

(defn
  avg
  [a-seq]
  (loop [r-seq a-seq
         sum 0
         index 0]
    (cond
      (empty? r-seq) (/ sum index)
      :else (recur (rest r-seq) (+ sum (first r-seq)) (inc index)))))

(defn
  parity
  [a-seq]
  (loop [r-seq a-seq
         result #{}]
    (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (cond
      (empty? r-seq) result
      :else (recur (rest r-seq) (toggle result (first r-seq)))))))

(defn
  fast-fibo
  [n]
  (loop [i 0
         i+1 1
         k 0]
    (cond
      (= k n) i
      :else (recur i+1 (+ i i+1) (inc k)))))

(defn
  cut-at-repetition
  [a-seq]
  (loop [result []
         r-seq a-seq
         used #{}]
    (cond
      (empty? r-seq) result
      (contains? used (first r-seq)) result
      :else (recur (conj result (first r-seq)) (rest r-seq) (conj used (first r-seq))))))

