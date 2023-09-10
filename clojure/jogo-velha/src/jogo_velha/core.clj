(ns jogo-velha.core
  (:use [jogo-velha.utils])
  (:require [clojure.string :as string]))

(defn show-board [board]
  (loop [lines board]
    (if (not-empty lines)
      (let [line (first lines)]
        (println line)
        (recur (rest lines))))))

(defn get-input [message]
  (println message)
  (read-line))

(defn is-valid-move [board indexes]
  (and (every? #(in-range % 0 2) indexes)
              (= "." (get-elem board indexes))))

(defn validate-input [input]
  (and (= (count input) 2) (is-numeric input)))

(defn make-move [board player move]
  (assoc-in board move player))

(defn winner? [board player]
  (let [victory-possibilities (get-all-victory-possibilities board)]
    (some true? (map #(victory? % player) victory-possibilities))))

(defn choose-next-player [current-player]
  (if (= current-player "X")
    "O"
    "X"))

(defn choose-next-board [victory is-draw new-board]
  (if (or victory is-draw)
    (create-board)
    new-board))

(defn update-scores [victory is-draw scores player]
  (if (or (not victory) is-draw)
    scores
    (into scores [[player (inc (scores player))]])))

(defn wanna-play? [victory is-draw]
  (if (or victory is-draw)
    (= (string/lower-case (get-input "Wanna play another game? (y/n)")) "y")
    true))

(defn -main []
  (loop [board (create-board)
         player "X"
         scores {"X" 0 "O" 0}
         playing true]
    (if playing
      (do
        (show-board board)
        (let [move (vec (get-input (format "Mover for player %s: " player)))]
          (if (validate-input move)
            (let [indexes (get-indexes move)]
             (if (is-valid-move board indexes)
              (let [new-board (make-move board player indexes)
                    victory (winner? new-board player)
                    is-draw (and (not victory) (is-board-full board))
                    next-player (choose-next-player player)
                    next-board (choose-next-board victory is-draw new-board)
                    next-scores (update-scores victory is-draw scores player)]
                (cond
                  victory (println (format "Player %s is the winner" player))
                  is-draw (println "No one wins"))
                (recur next-board next-player next-scores (wanna-play? victory is-draw)))
              (do
                (println (format "Invalid move"))
                (recur board player scores playing))))
            (do
              (println (format "Invalid input"))
              (recur board player scores playing)))))
      (println "End game: " scores))))
