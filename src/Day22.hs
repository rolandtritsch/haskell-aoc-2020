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

import Data.List.Split (splitOn)
import Data.Text (pack, unpack)
import Text.Regex.Pcre2 (matchAll)

import Util (inputRaw)
import Prelude

type Card = Int

data Game = Game [Card] [Card]
  deriving (Eq, Show)

-- | Read the input file and return the game to play.
input :: String -> Game
input filename = Game player1' player2'
  where
    (player1', player2') = (buildDeck p1, buildDeck p2)
    (p1 : p2 : _) = map unpack $ matchAll (pack "(Player [12]:\n)([0-9]*\n)*") (pack $ inputRaw filename)
    buildDeck p = map read $ init $ tail $ splitOn "\n" p

-- | Return/Calculate the score for a given deck. 
score :: [Int] -> Int
score deck = sum $ map (\(a, b) -> a * b) $ zip deck (reverse [1 .. (length deck)])

-- | Play one round, recursively (until we are done). 
playRound :: Game -> Game
playRound game@(Game player1 player2)
  | null player1 || null player2 = game
  | otherwise = playRound nextGame
  where
    cards = (head player1, head player2)
    nextGame = playCards cards (tail player1) (tail player2)
      where
        playCards (card1, card2) deck1 deck2
          | card1 > card2 = Game (deck1 ++ [card1, card2]) deck2
          | otherwise = Game deck1 (deck2 ++ [card2, card1])

-- | Solve part1.
part1 :: Game -> Int
part1 game = winingScore done1 done2
  where
    winingScore deck1 [] = score deck1
    winingScore [] deck2 = score deck2
    winingScore _ _ = 0
    (Game done1 done2) = playRound game

-- | Play one round, recursively (until we are done).
playRound2 :: Game -> Game
playRound2 game@(Game player1 player2)
  | null player1 || null player2 = game
  | otherwise = playRound2 nextGame
  where
    cards = (head player1, head player2)
    nextGame = playCards cards (tail player1) (tail player2)
      where
        playCards (card1, card2) deck1 deck2
          | card1 > card2 = Game (deck1 ++ [card1, card2]) deck2
          | otherwise = Game deck1 (deck2 ++ [card2, card1])

-- | Solve part2.
part2 :: Game -> Int
part2 game = winingScore done1 done2
  where
    winingScore deck1 [] = score deck1
    winingScore [] deck2 = score deck2
    winingScore _ _ = 0
    (Game done1 done2) = playRound2 game
