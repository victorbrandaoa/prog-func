(ns jogo-velha.utils)

(defn create-board []
  [["." "." "."]
   ["." "." "."]
   ["." "." "."]])

(defn get-indexes [move]
  (map #(Character/digit % 10) move))

(defn in-range [num start end]
  (<= start num end))

(defn is-numeric [move]
  (reduce #(and % %2) (map #(Character/isDigit %) move)))

(defn get-columns [board]
  (apply mapv vector board))

(defn get-primary-diagonal [board]
  (mapv #(get-in board [% %]) (range 3)))

(defn get-secondary-diagonal [board]
  (mapv #(get-in board [% (- 3 (inc %))]) (range 3)))

(defn get-all-victory-possibilities [board]
  (into (into board (get-columns board)) [(get-primary-diagonal board) (get-secondary-diagonal board)]))

(defn victory? [possibilitie last-player]
  (every? #(= % last-player) possibilitie))

(defn is-board-full [board]
  (every? #(not= "." %) (apply concat board)))

(defn get-elem
  [board indexes]
  (get-in board indexes))
