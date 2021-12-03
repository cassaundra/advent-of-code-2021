{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day02
  ( Movement,
    Direction,
    Distance,
    toSumTuple,
    pMovementFile,
  )
where

import Data.Functor (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Monoid

-- parser? overkill? what? huh?

type Parser = Parsec Void String

data Direction = Forward | Up | Down
  deriving (Eq, Show)

type Distance = Int

data Movement = Movement Direction Distance
  deriving (Eq, Show)

toSumTuple :: Movement -> (Sum Distance, Sum Distance)
toSumTuple (Movement Forward dist) = (Sum dist, 0)
toSumTuple (Movement Up dist) = (0, Sum (-dist))
toSumTuple (Movement Down dist) = (0, Sum dist)

pEntry :: Parser Direction
pEntry =
  choice
    [ Forward <$ string "forward",
      Up <$ string "up",
      Down <$ string "down"
    ]

pMovement :: Parser Movement
pMovement = do
  direction <- pEntry <?> "direction"
  char ' '
  distance <- L.decimal <?> "distance"
  return $ Movement direction distance

pMovementFile :: Parser [Movement]
pMovementFile = do
  sepEndBy pMovement eol <* eof
