{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module Main where

import Control.Monad
import Text.Printf
import Data.List

score :: [(String,[String])] -> ([String], [String], String, String) -> Maybe Int
score rankedChoices (group1, group2, game1, game2) = do
  scores1 <-forM group1 $ \player -> do
    ranks <- lookup player rankedChoices
    elemIndex game1 ranks
  scores2 <-forM group2 $ \player -> do
    ranks <- lookup player rankedChoices
    elemIndex game2 ranks
  return $ (sum (map (\x -> x * x) scores1)) + (sum (map (\x -> x * x) scores2))

minEntropy :: Int -> Int -> [String] -> [String] -> [(String, [String])] ->
              [(Maybe Int, ([String], [String], String, String))]
minEntropy n k players games rankedChoices =
  let options :: [([String], [String], String, String)] =
        [ (group1, group2, game1, game2)
        | group1 <- filter ((==k) . length) $ subsequences players
        , group2 <- [players \\ group1]
        , game1 <- games
        , game2 <- games
        ]
      scores = map (score rankedChoices) options
  in
  sort $ zip scores options 
  
main = do
  putStr "Number of Players: "
  n <- read @Int <$> getLine
  names <- forM [1..n] $ \i -> do
    putStr $ printf "Player %d name: " i
    getLine
  putStr "Number of Players in game 1: "
  k <- read @Int <$> getLine
  choices <- forM names $ \name -> do
    putStr $ printf "%s's choice: " name
    getLine
  putStrLn "Choices are:"
  let dict :: [(String, Int)] = zip choices [1..]
  forM dict $ \(s,i) -> putStrLn $ printf "%d: %s" i s
--  putStrLn "Number of choices per player: "
--  r <- read @Int <$> getLine
  putStrLn "On to ranking choices..."
  ranksPerPlayer <- forM names $ \name -> do
    putStrLn $ printf "Player %s:" name
    ranks <- forM [1..n] $ \i ->
               let validateChoice = do
                     putStr $ printf "  Choice #%d: " i
                     choice <- getLine
                     if any (==choice) choices then return choice
                     else putStrLn "Choice not found. Try again." >> validateChoice
               in
               validateChoice
    return ranks
  putStrLn $ unlines $ map show $ minEntropy n k names choices (zip names ranksPerPlayer)
