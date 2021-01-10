{-|
Problem: <https://adventofcode.com/2020/day/22>

Solution:

General - Playing cards. With 2 players. In rounds. Recursivly
until one of the players has no more cards to play.

Part 1 - Found out who won and calculate the score.

Part 2 - ???
-}
module Day22 where

import Prelude

import qualified Text.Regex as R
import Util (inputRaw)

data Game = Game {
    player1 :: [Int],
    player2 :: [Int]
}

input :: String -> Game
input filename = Game {player1 = player1', player2 = player2'} where
    contents = toStr $ unsafePerformIO $ readFile filename
    (player1', player2') = (buildDeck tokens 0,  buildDeck tokens 3) where
      linePattern = R.mkRegex "(Player [12]:\\n([0-9]*\\n)*)"
      tokens = R.matchRegex linePattern contents
      buildDeck ts i = map read $ lines $ tokens !! i

score :: [Int] -> Int
score deck = sum $ map (\(a, b) -> a * b) $ zip deck (reverse [1..(length deck)])

isEmpty :: forall a. [a] -> Bool
isEmpty [] = true
isEmpty _  = false

playRound :: Game -> Game
playRound game 
    | isEmpty game.player1 || isEmpty game.player2 = game
    | otherwise = playRound nextGame where
        cards = (head $ game.player1, head $ game.player2)
        nextGame = playCards cards (tail game.player1) (tail game.player2) where
            playCards (card1, card2) deck1 deck2
                | card1 > card2 = {player1 = deck1 ++ [card1, card2], player2 = deck2}
                | otherwise = {player1 = deck1, player2 = deck2  ++ [card2, card1]}

part1 :: Game -> Int
part1 game = winingScore done.player1 done.player2 where
    winingScore deck1 [] = score deck1 
    winingScore [] deck2 = score deck2 
    winingScore _ _ = 0 
    done = playRound game

part2 :: Game -> Int
part2 game = length (player1 game)
