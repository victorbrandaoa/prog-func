module Main (main) where

import Lib
import Data.Char (digitToInt)

showBoard :: [[String]] -> IO()
showBoard = mapM_ (\line -> putStrLn ("[" ++ unwords (map show line) ++ "]"))

getInput :: String -> IO String
getInput message = do
  putStrLn message
  input <- getLine
  return input

validateInput :: String -> Bool
validateInput input = (length input == 2) && (isNumeric input)

isValidMove :: [[String]] -> [Int] -> Bool
isValidMove board indexes = (all (\num -> (inRange num 0 2)) indexes) && ((getElem board indexes) == ".")

chooseNextPlayer :: String -> String
chooseNextPlayer "X" = "O"
chooseNextPlayer "O" = "X"

isWinner :: [[String]] -> String -> Bool
isWinner board player = any (== True) (map (\p -> isVictory p player) (getAllVictoryPossibilities board))

editRow :: [String] -> String -> Int -> [String]
editRow (_:xs) player 0 = player:xs
editRow (x:xs) player i = [x] ++ (editRow xs player (i - 1))

makeMove :: [[String]] -> String -> Int -> Int -> [[String]]
makeMove (x:xs) player 0 j = (editRow x player j):xs
makeMove (x:xs) player i j = [x] ++ (makeMove xs player (i - 1) j)

chooseNextBoard :: Bool -> Bool -> [[String]] -> [[String]]
chooseNextBoard victory isDraw newBoard | victory || isDraw = createBoard
                                        | otherwise = newBoard

castToInt :: String -> [Int]
castToInt move = (map digitToInt move)

wannaPlay :: Bool -> Bool -> Bool
wannaPlay victory isDraw = True

updateScores :: Bool -> Bool -> String -> String -> String
updateScores victory isDraw scores player = "SCORES"

main :: IO ()
main = do
  start (createBoard) "X" "SCORES" True

start :: [[String]] -> String -> String -> Bool -> IO()
start board player scores playing = do
  if playing then do
    showBoard board
    input <- (getInput ("Move for player " ++ player ++ ": "))
    if (validateInput input) then do
      let indexes = castToInt input

      if (isValidMove board indexes) then do
        let i = (indexes !! 0)
        let j = (indexes !! 1)
        let newBoard = makeMove board player i j
        let victory = isWinner newBoard player
        let isDraw = (not victory) && (isBoardFull board)
        let nextPlayer = chooseNextPlayer player
        let nextBoard = chooseNextBoard victory isDraw newBoard
        let nextScores = updateScores victory isDraw scores player

        if victory then do
          putStrLn ("Player " ++ player ++ " is the winner")
        else if isDraw then do
          putStrLn "No one wins"
        else do
          putStrLn ""
        
        start nextBoard nextPlayer nextScores (wannaPlay victory isDraw)
      else do
        putStrLn "Invalid move"
        start board player scores playing

    else do
      putStrLn "Invalid input"
      start board player scores playing
  else
    putStrLn ("End game: " ++ scores)
