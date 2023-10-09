(ns playground.core)

(defn my-map [func list]
  (loop [list list
         resp []]
    (if (empty? list)
      resp
      (recur (rest list) (conj resp (func (first list)))))))

(defn my-filter [func list]
  (loop [list list
         resp []]
    (let [head (first list)]
      (cond
        (empty? list) resp
        (func head) (recur (rest list) (conj resp head))
        :else (recur (rest list) resp)))))

(defn my-reduce 
  ([list func] (my-reduce (rest list) func (first list)))
  ([list func acc]
  (if (empty? list)
    acc
    (recur (rest list) func (func (first list) acc)))))

(defn factorial-v1 [n]
  (if (= n 1N)
    1N
    (* n (factorial-v1 (dec n)))))

(defn factorial-rec-v1 [n acc]
  (if (= n 1N)
    acc
    (factorial-rec-v1 (dec n) (* n acc))))

(defn factorial-v2 [n]
  (factorial-rec-v1 n 1N))

(defn factorial-rec-v2 [n acc]
  (if (= n 1N)
    acc
    (fn [] (factorial-rec-v2 (dec n) (* n acc)))))

(defn factorial-v3 [n]
  (trampoline (factorial-rec-v2 n 1N)))

(defn factorial-v4 [n]
  (loop [n n
         acc 1N]
    (if (= n 1N)
      acc
      (recur (dec n) (* n acc)))))

(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))

(def primes (filter prime? (drop 2 (range))))

(defn fib-seq-generator []
  (letfn [(fib-gen [a b]
            (lazy-seq (cons a (fib-gen b (+ a b)))))]
    (fib-gen 0N 1N)))

(def fib-seq (fib-seq-generator))

(def celebs
  '({:name "Taylor Swift" :course "Songwriting" :grade ##Inf :money 30000000}
    {:name "Robert Downey Jr." :course "Acting" :grade 10 :money 20000000}
    {:name "Jake Gyllenhaal" :course "Acting" :grade 4 :money 1000}
    {:name "Olivia Rodrigo" :course "Songwriting" :grade 8 :money 100000}
    {:name "Bruno Mars" :course "Songwriting" :grade 10 :money 10000000}
    {:name "Shawn Mendes" :course "Songwriting" :grade 8 :money 1000000}
    {:name "Travis Kelce" :course "Football player" :grade 10 :money 1000000}
    {:name "Joe Burrow" :course "Football player" :grade 10 :money 1000000}))

(defn filter-by-songwriters [data] (filter #(= (:course %) "Songwriting") data))
(defn get-money [data] (map :money data))
(defn sum-money [data] (reduce + data))
(defn sum-songwriters-money []
  (comp sum-money get-money filter-by-songwriters))

;(reduce + (map :money (filter #(= (:course %) "Songwriting") celebs)))
;(->> celebs (filter #(= (:course %) "Songwriting")) (map :money) (reduce +))
;(-> celebs filter-by-songwriters get-money sum-money)
;((sum-songwriters-money) celebs)