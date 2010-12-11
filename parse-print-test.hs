module Main( main ) where

import System (getArgs,getProgName)
import System.Exit
import System.IO
import Gaf.Parser (readGSchem)
import Gaf.ShowGSchem (showGSchem)

main = do
  pn <- getProgName
  args <- getArgs
  if (any (=="--help") args) then putStrLn ("Usage: " ++ pn ++ " [INPUT] [OUTPUT]") 
                                  >> exitSuccess
                             else return ()
  let in_fn = if (length args >= 1) then (args !! 0) else "-"
  let out_fn = if (length args >= 2) then (args !! 2) else "-"
  input <- case in_fn of { "-" -> return stdin
                         ;  _  -> openFile in_fn ReadMode }
  output <- case out_fn of { "-" -> return stdout
                           ;  _  -> openFile in_fn WriteMode }
  contents <- hGetContents input
  let parse = readGSchem contents
  case parse of
    Left err -> hPutStrLn stderr ("Error reading " ++ rn in_fn ++ ":" ++ show err)
                >> exitFailure
    Right gs -> hPutStrLn output $ showGSchem gs
  hClose input
  hClose output
  exitSuccess
  where
     rn in_fn = case in_fn of {"-" -> "stdin" ; fn  -> fn}