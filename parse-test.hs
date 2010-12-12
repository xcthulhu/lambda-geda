module Main( main ) where

import System (getArgs)
import System.IO
import Control.Monad (forM_,liftM)
import Geda.Parser (readGSchem)

tryParse :: String -> String -> IO ()
tryParse fn x = case readGSchem x of
    Left err -> hPutStrLn stderr $ fn ++ " FAILED!\nReason: " ++ show err
    Right _ -> putStrLn $ fn ++ " PASSED!"

main :: IO ()
main = do
  args <- getArgs
  forM_ args printFile
  where
    printFile fn = do
      handel <- openFile fn ReadMode
      contents <- hGetContents handel
      tryParse fn contents
      hClose handel
