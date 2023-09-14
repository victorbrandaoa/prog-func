module Main (main) where

import Lib
import Data.Char (digitToInt)
import Data.Map (Map, fromList, insertWith)
import Control.Monad (when)

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

wannaPlay :: Bool -> Bool -> IO Bool
wannaPlay victory isDraw = do
  if (victory || isDraw) then do
    input <- (getInput "Wanna play another game? (y/n)")
    return (input == "y")
  else do
    return True

updateScores :: Bool -> Bool -> Map String Int -> String -> Map String Int
updateScores victory isDraw scores player | (not victory) || isDraw = scores
                                          | otherwise = insertWith (+) player 1 scores

main :: IO ()
main = do
  start (createBoard) "X" (fromList [("X", 0), ("O", 0)]) True

start :: [[String]] -> String -> Map String Int -> Bool -> IO()
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
        let isDraw = (not victory) && (isBoardFull newBoard)
        let nextPlayer = chooseNextPlayer player
        let nextBoard = chooseNextBoard victory isDraw newBoard
        let nextScores = updateScores victory isDraw scores player

        when victory $ do
          putStrLn ("Player " ++ player ++ " is the winner")

        when isDraw $ do
          putStrLn "No one wins"
        
        continuePlaying <- (wannaPlay victory isDraw)
        start nextBoard nextPlayer nextScores continuePlaying
      else do
        putStrLn "Invalid move"
        start board player scores playing

    else do
      putStrLn "Invalid input"
      start board player scores playing
  else
    putStrLn ("End game: " ++ (show scores))
