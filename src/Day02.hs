{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day02
  ( Direction,
    Distance,
    Movement,
    Submarine,
    defaultSubmarine,
    moveSub,
    pMovementFile,
    subDepth,
    subHoriz,
  )
where

import Control.Lens
import Data.Functor (void)
import Data.Monoid
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- parser? overkill? what? huh?

type Parser = Parsec Void String

data Direction = Forward | Up | Down
  deriving (Eq, Show)

type Distance = Int

data Movement = Movement Direction Distance
  deriving (Eq, Show)

data Submarine = Submarine
  { _subDepth :: Int,
    _subHoriz :: Int
  }
  deriving (Eq, Show)

makeLenses ''Submarine

defaultSubmarine = Submarine {_subDepth = 0, _subHoriz = 0}

moveSub :: Movement -> Submarine -> Submarine
moveSub (Movement dir dist) sub = case dir of
  Forward -> sub & subHoriz +~ dist
  Up -> sub & subDepth -~ dist
  Down -> sub & subDepth +~ dist

pDirection :: Parser Direction
pDirection =
  choice
    [ Forward <$ string "forward",
      Up <$ string "up",
      Down <$ string "down"
    ]
    <?> "direction"

pMovement :: Parser Movement
pMovement = do
  direction <- pDirection
  char ' '
  distance <- L.decimal <?> "distance"
  return $ Movement direction distance

pMovementFile :: Parser [Movement]
pMovementFile = do
  sepEndBy pMovement eol <* eof
