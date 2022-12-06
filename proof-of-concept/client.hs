module Main where

import Network.Socket
import System.IO
import Control.Monad (forever)
import qualified Data.List as L
import GHC.Base (when)
import Data.Text as T
import Data.Char (isDigit, isLetter)

main :: IO ()
main = do
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
    then do putStrLnFlush $ word ++ " challenges you to a battle!"
            startGame hdl
    else readOpponent hdl word

startGame :: Handle -> IO ()
startGame hdl = do
    let word = "BEGIN"  -- TODO: sample random correct word
    hPutStrLn hdl $ "001 " ++ word ++ " "
    guessRoutine hdl word

readOpponent :: Handle -> String -> IO ()
readOpponent hdl word = do
    (code, oppWord) <- readAndParseFlagged hdl
    if code == "002"
    then startGame hdl
    else do putStrLnFlush $ "Opponent guessed: " ++ oppWord
            guessRoutine hdl word

guessRoutine :: Handle -> String -> IO ()
guessRoutine hdl word = do
    outputWord <- getGuess
    if outputWord == word
    then do hPutStrLn hdl "002 ***** "
            (_, word) <- readAndParseFlagged hdl
            readOpponent hdl word
    else do hPutStrLn hdl $ "003 " ++ outputWord ++ " "
            readOpponent hdl word

-- prompts user for guess, repeats if guess is not length 5
getGuess :: IO String
getGuess = do
    putStrFlush "guess: "
    guess <- getLine
    if Prelude.length guess == 5
    then return $ applyText toUpper guess
    else getGuess

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
    else do msg <- hGetLine hdl
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