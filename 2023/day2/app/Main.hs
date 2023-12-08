{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Text.Megaparsec as M hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.ParserCombinators.ReadP as L
import Data.Char (isSpace)
import Data.Maybe (catMaybes)

type Parser = M.Parsec Void Text

data Color = Blue | Red | Green deriving (Show, Eq)
type Turn = [Draw]

data Game = Game {
  gameId :: Int, 
  turns :: [Turn]
} deriving (Show, Eq)

data Draw = Draw {
  color :: Color, 
  count :: Int
} deriving (Show, Eq)

parseColor :: Parser Color
parseColor = M.choice
  [ Blue <$ string "blue"
  , Red  <$ string "red"
  , Green <$ string "green"
  ]

parseDraw :: Parser Draw
parseDraw = do 
  count <- L.decimal 
  color <- parseColor
  return Draw {..}

parseRow :: Parser Game
parseRow = do
  _ <- string "Game"
  gameId <- L.decimal
  _ <- char ':'
  turns <- (parseDraw `M.sepBy` char ',') `M.sepBy` char ';'
  return Game {..}

maximumNumberForColor :: Color -> Int 
maximumNumberForColor Blue = 14
maximumNumberForColor Green = 13
maximumNumberForColor Red = 12

isDrawPossible :: Draw -> Bool 
isDrawPossible draw = count draw <= maximumNumberForColor (color draw)

isGamePossible :: Game -> Bool 
isGamePossible game = all (all isDrawPossible) (turns game)

sumOfPossibleGameIds :: [Maybe Game] -> Int 
sumOfPossibleGameIds games = sum (map gameId (catMaybes (filter (maybe False isGamePossible) games)))

processString :: String -> Maybe Game
processString input = case M.parse (parseRow <* M.eof) "" cleanedInput of
  Left _err -> Nothing
  Right result -> Just result
  where
    cleanedInput = T.pack (filter (not . isSpace) input)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let linesList = lines contents
  let parsedResults = map processString linesList
  putStrLn "Parsed Objects:"
  let sumResult = sumOfPossibleGameIds parsedResults
  putStrLn $ "Sum of possible game IDs: " ++ show sumResult
