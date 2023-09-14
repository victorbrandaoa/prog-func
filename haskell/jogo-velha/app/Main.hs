module Main (main) where

import Lib

printMatrixLines :: [[String]] -> IO()
printMatrixLines = mapM_ (\line -> putStrLn ("[" ++ unwords (map show line) ++ "]"))

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

main :: IO ()
main = do
  let m = createBoard
  printMatrixLines m
  -- teste <- getInput "Teste: "
  -- putStrLn "------------"
  -- putStrLn ("[" ++ unwords (map show (getPrimaryDiagonal m)) ++ "]")
  -- putStrLn ("[" ++ unwords (map show (getSecondaryDiagonal m)) ++ "]")
  putStrLn "Fim"
