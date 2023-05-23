{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLists, ViewPatterns #-}


module Domain where

import Data.ByteString.Lazy.UTF8
import Data.Char
import Data.Set
import Text.Read (readEither)

data Colour = White | Black deriving (Eq, Show)

data Move =
    Move MoveInfo
  | Capture MoveInfo
  | Check MoveInfo
  | Checkmate MoveInfo
  | Stalemate MoveInfo

data MoveInfo = MoveInfo
  { piece :: Piece 
  , colour :: Colour
  , position :: Position }

type ParserError = String

newtype ChessMoveParser a = ChessMoveParser 
  { runParser :: ByteString -> Either ParserError (ByteString, a) }

parseRank :: Char -> ChessMoveParser Char
parseRank c =
  let ranks = ['a'..'h'] :: Set Char
   in ChessMoveParser $ \case
    (uncons -> Nothing) -> Left "Invalid rank"
    (uncons -> Just (x, xs)) ->
      if c `member` ranks && c == x
      then Right (xs, c)
      else Left "Invalid rank"

parseFile :: Int -> ChessMoveParser Int
parseFile c =
  let files = [1..8] :: Set Int
   in ChessMoveParser $ \case
    (uncons -> Nothing) -> Left "Invalid file"
    (uncons -> Just (x, xs)) ->
      let cInt = readEither [x] :: Either String Int
       in case cInt of
        Left l -> Left l
        Right i ->
          if i `member` files && i == c
          then Right (xs, i)
          else Left "Invalid file"

parsePosition :: ChessMoveParser Position
parsePosition = ChessMoveParser $ readEither . fmap toLower . toString

data Piece =
    Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving Eq

instance Show Piece where
  show :: Piece -> String
  show Pawn = ""
  show Knight = "N"
  show Bishop = "B"
  show Rook = "R"
  show Queen = "Q"
  show King = "K"

data Position =
   A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8 |
   A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7 |
   A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6 |
   A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5 |
   A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4 |
   A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3 |
   A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2 |
   A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1
   deriving (Eq, Show, Read)
