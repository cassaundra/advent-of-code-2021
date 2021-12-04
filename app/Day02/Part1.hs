module Day02.Part1 where

import Control.Lens.Operators
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (Foldable (foldl))
import Data.Monoid
import Day02
import System.Environment (getArgs)
import Text.Megaparsec

main :: IO ()
main = do
  fileContent <- getArgs >>= readFile . head
  let movements = runParser pMovementFile "" fileContent
  case movements of
    Left err -> putStrLn $ errorBundlePretty err
    Right movements -> do
      let sub = foldl (flip moveSub) defaultSubmarine movements
      print $ sub ^. subDepth * sub ^. subHoriz
