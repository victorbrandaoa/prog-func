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

(defn my-reduce [list func acc]
  (if (empty? list)
    acc
    (recur (rest list) func (func (first list) acc))))

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

(def primes (filter prime? (range)))

(defn fib-seq []
  (letfn [(fib-gen [a b]
            (lazy-seq (cons a (fib-gen b (+ a b)))))]
    (fib-gen 0 1)))
