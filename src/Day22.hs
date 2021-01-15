-- |
-- Problem: <https://adventofcode.com/2020/day/22>
--
-- Solution:
--
-- General - Playing cards. With 2 players. In rounds. Recursivly
-- until one of the players has no more cards to play.
--
-- Part 1 - Found out who won and calculate the score.
--
-- Part 2 - ???
module Day22 where

import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Text.Regex.PCRE ((=~))
import Util (inputRaw)
import Prelude

data Game = Game
  { player1 :: [Int],
    player2 :: [Int]
  }

input :: String -> Game
input filename = Game player1' player2'
  where
    (player1', player2') = (buildDeck p1, buildDeck p2)
    (p1 : p2 : _) = (inputRaw filename) =~ "(Player [12]:\n)([0-9]*\n)*" :: [[String]]
    buildDeck (match : prefix : _) = map read $ init $ splitOn "\n" $ fromJust $ stripPrefix prefix match
    buildDeck _ = error "Bad deck"

score :: [Int] -> Int
score deck = sum $ map (\(a, b) -> a * b) $ zip deck (reverse [1 .. (length deck)])

playRound :: Game -> Game
playRound game
  | null (player1 game) || null (player2 game) = game
  | otherwise = playRound nextGame
  where
    cards = (head (player1 game), head (player2 game))
    nextGame = playCards cards (tail (player1 game)) (tail (player2 game))
      where
        playCards (card1, card2) deck1 deck2
          | card1 > card2 = Game {player1 = deck1 ++ [card1, card2], player2 = deck2}
          | otherwise = Game {player1 = deck1, player2 = deck2 ++ [card2, card1]}

part1 :: Game -> Int
part1 game = winingScore (player1 done) (player2 done)
  where
    winingScore deck1 [] = score deck1
    winingScore [] deck2 = score deck2
    winingScore _ _ = 0
    done = playRound game

part2 :: Game -> Int
part2 game = length (player1 game)
