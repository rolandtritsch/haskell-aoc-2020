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

-- | Play game (by playing rounds recursively)
playGame :: Game -> Game
playGame game@(Game player1 player2)
  | null player1 || null player2 = game
  | otherwise = playGame $ playRound game

-- | Play one round.
playRound :: Game -> Game
playRound (Game player1 player2) = nextGame
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
    (Game done1 done2) = playGame game

-- | Play game (by playing rounds recursively) (part2).
playGame2 :: [Game] -> Game -> ([Game], Game)
playGame2 previous game@(Game player1 player2)
  | elem game previous = (previous, Game (player1 ++ player2) [])
  | null player1 || null player2 = (previous, game)
  | otherwise = playGame2 (game : previous) $ playRound2 game

-- | Play one round (part2).
playRound2 :: Game -> Game
playRound2 game@(Game player1 player2) = nextGame
  where
    nextGame = playCards (head player1) (head player2) (tail player1) (tail player2)
      where
        playCards card1 card2 deck1 deck2
          | card1 <= (length deck1) && card2 <= (length deck2) = playSubGame2 game 
          | card1 > card2 = Game (deck1 ++ [card1, card2]) deck2
          | otherwise = Game deck1 (deck2 ++ [card2, card1])

-- Play a subgame (part2).
playSubGame2 :: Game -> Game
playSubGame2 (Game player1 player2) = nextGame
  where
    (_, result) = playGame2 [] (Game (tail player1) (tail player2))
    nextGame = playCards (head player1) (head player2) (tail player1) (tail player2)
      where
        playCards card1 card2 deck1 deck2
          | player1wins result = Game (deck1 ++ [card1, card2]) deck2
          | otherwise = Game deck1 (deck2 ++ [card2, card1])
        player1wins (Game _ []) = True
        player1wins _ = False

-- | Solve part2.
part2 :: Game -> Int
part2 game = winingScore done1 done2
  where
    winingScore deck1 [] = score deck1
    winingScore [] deck2 = score deck2
    winingScore _ _ = 0
    (_, (Game done1 done2)) = playGame2 [] game
