module Client where

import Control.Monad (forever)
import Data.Char (isDigit, isLetter, toUpper)
import Data.List qualified as L
import Data.Map as M
import Data.Text as T
import GHC.Base (when)
import Network.Socket
import System.IO
import System.Random

main :: IO ()
main = do
  filecontents <- readFile "official.txt"
  let dict = Prelude.lines $ applyText T.toUpper filecontents
  -- connect socket
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  connect sock (SockAddrInet 4242 0x0100007f)
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  -- receive input username message
  msg <- hGetLine hdl
  putStrLnFlush msg
  -- respond with username
  name <- getLine
  hPutStrLn hdl name
  -- receive greeting
  greet <- hGetLine hdl
  putStrLnFlush greet
  (code, word) <- readAndParseFlagged hdl
  if code == "000"
    then do
      putStrLnFlush $ word ++ " challenges you to a battle!"
      startGame hdl dict
    else readOpponent hdl word dict

startGame :: Handle -> [String] -> IO ()
startGame hdl dict = do
  randomWord <- pollRandomWord dict
  hPutStrLn hdl $ "001 " ++ randomWord ++ " "
  guessRoutine hdl randomWord dict

readOpponent :: Handle -> String -> [String] -> IO ()
readOpponent hdl word dict = do
  (code, oppWord) <- readAndParseFlagged hdl
  if code == "002"
    then do
      putStrLnFlush "New game starting"
      startGame hdl dict
    else do
      putStrLnFlush $ "Opponent guessed: " ++ oppWord
      r <- guessResult oppWord word
      putStrLnFlush $ "                  " ++ r
      guessRoutine hdl word dict

guessRoutine :: Handle -> String -> [String] -> IO ()
guessRoutine hdl word dict = do
  outputWord <- getGuess dict
  r <- guessResult outputWord word
  putStrLnFlush $ "       " ++ r
  if outputWord == word
    then do
      putStrLnFlush "You've won!"
      putStrLnFlush "New game starting!  Your opponent will start first"
      hPutStrLn hdl "002 ***** "
      (_, word) <- readAndParseFlagged hdl
      readOpponent hdl word dict
    else do
      hPutStrLn hdl $ "003 " ++ outputWord ++ " "
      readOpponent hdl word dict

-- generates a result string where each char of the string indicates that
-- the corresponding guess char at that position is
-- 'O': in the correct spot
-- 'X': is not in the word
-- '%': in the wrong spot
guessResult :: String -> String -> IO String
guessResult guess word = do
  let (r1, misses) = markCorrect guess word "?????"
  let r2 = markNotInWord guess word r1
  return $ markWrongSpot guess misses r2

-- replaces chars in result string with 'O' if guess char is in the correct spot
markCorrect :: String -> String -> String -> (String, Map Char Int)
markCorrect [] _ _ = ("", M.empty)
markCorrect (g : gs) (w : ws) (r : rs)
  | g == w = ('O' : rs', miss)
  | otherwise = (r : rs', insertWith (+) w 1 miss)
  where
    (rs', miss) = markCorrect gs ws rs

-- replaces chars in result string with 'X' if guess char is not in the word
markNotInWord :: String -> String -> String -> String
markNotInWord [] _ _ = ""
markNotInWord (g : gs) word (r : rs)
  | g `Prelude.elem` word = r : rs'
  | otherwise = 'X' : rs'
  where
    rs' = markNotInWord gs word rs

-- replaces chars in result string with '%' if guess char is in the wrong spot
-- if there are duplicates of some guess char but only one copy of that char in
-- the word, only replace the leftmost char with '%' and the rest with 'X'
markWrongSpot :: String -> Map Char Int -> String -> String
markWrongSpot [] _ _ = ""
markWrongSpot (g : gs) miss (r : rs)
  | (r == 'O') || (r == 'X') = r : markWrongSpot gs miss rs
  | otherwise =
      if findWithDefault 0 g miss > 0
        then "%" ++ markWrongSpot gs (insertWith (+) g (-1) miss) rs
        else "X" ++ markWrongSpot gs miss rs

-- prompts user for guess, repeats if guess is not length 5
getGuess :: [String] -> IO String
getGuess dict = do
  putStrFlush "guess: "
  g <- getLine
  let guess = applyText T.toUpper g
  if Prelude.length guess == 5
  then if guess `Prelude.elem` dict
         then return $ applyText T.toUpper guess
         else do
           putStrLnFlush "Guess is not a valid word!"
           getGuess dict
  else do
    putStrLnFlush "Guess must be 5 letters long!"
    getGuess dict

-- reads message between flags, parses code and word
readAndParseFlagged :: Handle -> IO (String, String)
readAndParseFlagged hdl = do
  msg <- readBetween hdl "-->" "<--"
  return (parseCode msg, parseWord msg)

-- reads from handle until it receives message between flags
-- returns message between flags, without flags
readBetween :: Handle -> String -> String -> IO String
readBetween hdl f1 f2 = do
  eof <- hIsEOF hdl
  if eof
    then readBetween hdl f1 f2
    else do
      msg <- hGetLine hdl
      if (f1 `L.isPrefixOf` msg) && (f2 `L.isSuffixOf` msg)
        then return $ applyText (T.drop 3 . dropEnd 3) msg
        else readBetween hdl f1 f2

-- reads as many consecutive digits as possible
parseCode :: String -> String
parseCode = applyText (T.takeWhile isDigit . strip)

-- drops code, reads as many consecutive letters as possible
parseWord :: String -> String
parseWord = applyText (T.takeWhile isLetter . strip . T.dropWhile isDigit . strip)

-- automatically packs and unpacks string for Data.Text processing functions
applyText :: (Text -> Text) -> String -> String
applyText f str = unpack $ f $ pack str

-- outputs string and immediately flushes
putStrFlush :: String -> IO ()
putStrFlush str = do
  putStr str
  hFlush stdout

-- outputs string with newline and immediately flushes
putStrLnFlush :: String -> IO ()
putStrLnFlush str = do
  putStrLn str
  hFlush stdout

pollRandomWord :: [String] -> IO String
pollRandomWord dict = do
  gen <- initStdGen
  let (idx, _) = uniformR (0 :: Int, Prelude.length dict - 1 :: Int) gen
  return $ applyText T.toUpper (dict !! idx)