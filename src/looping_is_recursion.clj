(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? n)
                   acc
                   (recur (* acc k) (- n 1) k)))]
    (helper 1 exp base)))

(defn last-element [a-seq]
  (let [helper (fn [prev a-seq]
                 (if (empty? a-seq)
                   prev
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (and (empty? seq1) (empty? seq2)) true
                   (or (empty? seq1) (empty? seq2)) false
                   (not (= (first seq1) (first seq2))) false
                   :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         p pred
         a a-seq]
    (if (empty? a)
      nil
      (if (p (first a))
        i
        (recur (+ i 1) p (rest a))))))

(defn avg [a-seq]
  (loop [n 0
         s 0
         a a-seq]
    (if (empty? a)
      (if (zero? n)
        s
        (/ s n))
      (recur (+ n 1) (+ s (first a)) (rest a)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [s #{}
         a a-seq]
    (if (empty? a)
      s
      (recur (toggle s (first a)) (rest a)))))

(defn fast-fibo [n]
  (loop [k n
         fib1 0
         fib2 1]
    (if (zero? k)
      0
      (if (= k 1)
        fib2
        (recur (- k 1) fib2 (+ fib1 fib2))))))

(defn cut-at-repetition [a-seq]
  (loop [a a-seq
         e #{}
         r '[]]
    (if (empty? a)
      r
      (recur (rest a)
             (conj e (first a))
             (if (contains? e (first a))
               r
               (conj r (first a)))))))

