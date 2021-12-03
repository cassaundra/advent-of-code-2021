module Day02.Part1 where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (Foldable (foldl))
import Day02
import System.Environment (getArgs)
import Text.Megaparsec
import Data.Monoid

main :: IO ()
main = do
  fileContent <- getArgs >>= readFile . head
  let movements = runParser pMovementFile "" fileContent
  case movements of
    Left err -> putStrLn "error" -- TODO print formatted error
    Right movements -> do
      let result = foldMap toSumTuple movements
      print $ getSum (fst result) * getSum (snd result)
