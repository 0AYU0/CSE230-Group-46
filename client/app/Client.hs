{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Client where

import Control.Monad (forever, void)
import Data.Char (isDigit, isLetter, toUpper)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Base (when)
import Network.Socket
import System.IO
import System.Random
import Brick
import Brick.BChan
import qualified Graphics.Vty as V
import Lens.Micro.TH
import Lens.Micro.Mtl
import Foreign.StablePtr
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.IO.Class (MonadIO(liftIO))

data GameState
  = Intro
  | MyTurn
  | TheirTurn

data MessageEvent = Message String

data GlobalState =
  GlobalState { _stLines :: [String],
                _stInput :: String,
                _stHandle :: Handle,
                _stDict :: [String],
                _stCorrect :: String,
                _stGS :: GameState }
makeLenses ''GlobalState

drawUI :: GlobalState -> [Widget ()]
drawUI st = [ui]
  where
    lines = _stLines st
    widgs = map str lines
    ui = foldl (<=>) (str "") widgs

appEvent :: BrickEvent () MessageEvent -> EventM () GlobalState ()
appEvent e = do
    st <- get
    let hdl = (_stHandle st)
    let dict = (_stDict st)
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent (V.EvKey V.KBS []) -> do stLines %= deleteStrEnd
                                          stInput %= backspace
        VtyEvent (V.EvKey (V.KChar c) []) -> do stLines %= putStrEnd [c]
                                                stInput %= (++ [c])
        VtyEvent (V.EvKey V.KEnter []) -> do case (_stGS st) of 
                                               Intro -> do liftIO $ hPutStrLn (_stHandle st) (_stInput st)
                                                           putStrLnTUI ""
                                               MyTurn -> do let guess = applyText T.toUpper (_stInput st)
                                                            if length guess == 5
                                                            then if guess `elem` dict
                                                                 then do let word = (_stCorrect st)
                                                                         r <- liftIO $ guessResult guess word
                                                                         putStrLnTUI $ "       " ++ r
                                                                         if guess == word
                                                                         then do
                                                                           stLines .= []
                                                                           putStrLnTUI "You've won!"
                                                                           putStrLnTUI "New game starting!  Your opponent will start first"
                                                                           liftIO $ hPutStrLn hdl "002 ***** "
                                                                         else do
                                                                           liftIO $ hPutStrLn hdl $ "003 " ++ guess ++ " "
                                                                         stGS .= TheirTurn
                                                                 else do putStrLnTUI "Guess is not a valid word!"
                                                                         putStrTUI "guess: "
                                                            else do putStrLnTUI "Guess must be 5 letters long!"
                                                                    putStrTUI "guess: "
                                                            
                                               TheirTurn -> putStrLnTUI "It's not your turn yet! >:("
                                             stInput .= ""
        AppEvent (Message msg) -> case (_stGS st) of
                                    Intro -> if "-->" `L.isPrefixOf` msg
                                             then do let (code, word) = parseFlagged msg
                                                     if code == "000"
                                                     then do putStrLnTUI $ word ++ " challenges you to a battle!"
                                                             randomWord <- liftIO $ pollRandomWord (_stDict st)
                                                             stCorrect .= randomWord
                                                             liftIO $ hPutStrLn hdl $ "001 " ++ randomWord ++ " "
                                                             putStrTUI $ "guess: "
                                                             stGS .= MyTurn
                                                      else if code == "001"
                                                           then do stCorrect .= word
                                                                   stGS .= TheirTurn
                                                           else putStrLnTUI "how did you get here lol"
                                             else putStrLnTUI $ msg ++ "\n"
                                    TheirTurn -> do let (code, word) = parseFlagged msg
                                                    if code == "001"
                                                    then do stCorrect .= word
                                                            stGS .= TheirTurn
                                                    else if code == "002"
                                                         then do stLines .= []
                                                                 putStrLnTUI "You lost! :("
                                                                 putStrLnTUI "New game starting! You will start first"
                                                                 randomWord <- liftIO $ pollRandomWord (_stDict st)
                                                                 stCorrect .= randomWord
                                                                 liftIO $ hPutStrLn hdl $ "001 " ++ randomWord ++ " "
                                                                 putStrTUI $ "guess: "
                                                                 stGS .= MyTurn
                                                         else if code == "003"
                                                              then do putStrLnTUI $ "Opponent guessed: " ++ word
                                                                      r <- liftIO $ guessResult word (_stCorrect st)
                                                                      putStrLnTUI $ "                  " ++ r
                                                                      putStrTUI $ "guess: "
                                                                      stGS .= MyTurn
                                                              else putStrLnTUI "how did you get here lol"
        _ -> return ()

backspace :: String -> String
backspace s = if s == "" then "" else init s

putStrEnd :: String -> [String] -> [String]
putStrEnd new lines = (init lines) ++ [last lines ++ new]

deleteStrEnd :: [String] -> [String]
deleteStrEnd lines = (init lines) ++ [backspace $ last lines]

putStrTUI :: String -> EventM () GlobalState ()
putStrTUI msg = stLines %= putStrEnd msg

putStrLnTUI :: String -> EventM () GlobalState ()
putStrLnTUI msg = stLines %= (++ [msg, ""])

theApp :: App GlobalState MessageEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

main :: IO ()
main = do
  filecontents <- readFile "official.txt"
  let dict = lines $ applyText T.toUpper filecontents
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  connect sock (SockAddrInet 4242 0x0100007f)
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  let initialState = GlobalState [""] "" hdl dict "" Intro

  chan <- newBChan 10
  void $ forkIO $ forever $ do
    msg <- hGetLine hdl
    writeBChan chan (Message msg)

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) theApp initialState
  

-- startGame :: Handle -> [String] -> GlobalState -> IO ()
-- startGame hdl dict st = do
--   randomWord <- pollRandomWord dict
--   stCorrect .= randomWord
--   hPutStrLn hdl $ "001 " ++ randomWord ++ " "
--   guessRoutine hdl randomWord dict st

-- readOpponent :: Handle -> String -> [String] -> GlobalState -> IO ()
-- readOpponent hdl word dict st = do
--   -- (code, oppWord) <- readAndParseFlagged hdl
--   if code == "002"
--     then do
--       putStrLnTUI "New game starting"
--       startGame hdl dict st
--     else do
--       putStrLnTUI $ "Opponent guessed: " ++ oppWord
--       r <- guessResult oppWord word
--       putStrLnTUI $ "                  " ++ r
--       guessRoutine hdl word dict st

-- guessRoutine :: Handle -> String -> [String] -> GlobalState -> IO ()
-- guessRoutine hdl word dict st = do
--   outputWord <- getGuess dict st
--   r <- guessResult outputWord word
--   putStrLnTUI $ "       " ++ r
--   if outputWord == word
--     then do
--       putStrLnTUI "You've won!"
--       putStrLnTUI "New game starting!  Your opponent will start first"
--       hPutStrLn hdl "002 ***** "
--       -- (_, word) <- readAndParseFlagged hdl
--       -- readOpponent hdl word dict
--     else do
--       hPutStrLn hdl $ "003 " ++ outputWord ++ " "
--       -- readOpponent hdl word dict

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

prop_checkWordLetters :: Property
prop_checkWordLetters = forAll (genRandomString r) (\(first, second) -> countNumOverlap first second == countNumO (fst (markCorrect first second "?????")))
                          where
                            r = chooseInt(5,5)
-- Can randomize 5 to be some other letter
-- Check the map, ensure that the sum of entries is number of non-matching characters
-- If in the map, it's not in the second string
-- Ensure that guessResult has equivalent matches
prop_checkMoreWords :: Property
prop_checkMoreWords = forAll (genRandomString r) (\(first, second) -> countNumOverlap first second == countNumO (fst (markCorrect first second (clone 12 "?"))))
                        where
                          r = chooseInt(1, 10)

prop_checkMapEntries :: Property
prop_checkMapEntries = forAll (randomString 8) (\(first, second) -> countNumOverlap first second == 8 - countMap (snd (markCorrect first second (clone 12 "?"))))

prop_checkIndices :: Property
prop_checkIndices = forAll (genRandomString r) (\(first, second) -> (Prelude.length (countElements first second 0)) >= (Prelude.length (countMatches (markWrongSpot first (snd (markCorrect first second "??????????")) (markNotInWord first second (fst (markCorrect first second "??????????")))) 0)))
  where
    r = chooseInt(1,10)

prop_checkIndicesExist :: Property
prop_checkIndicesExist = forAll (genRandomString r) (\(first, second) -> countIndexExists (countElements first second 0) (countMatches (markWrongSpot first (snd (markCorrect first second "??????????")) (markNotInWord first second (fst (markCorrect first second "??????????")))) 0))
  where
    r = chooseInt(1,10)

countIndexExists :: [Int] -> [Int] -> Bool
countIndexExists _ [] = True
countIndexExists a (x:xs)
 | x `Prelude.elem` a = countIndexExists a xs
 | otherwise = False

countElements :: String -> String -> Int -> [Int]
countElements [] _ _ = []
countElements (x:xs) word idx
  | x `Prelude.elem` word = [idx] ++ (countElements xs word (idx + 1))
  | otherwise = countElements xs word (idx + 1)

countMatches :: String -> Int -> [Int]
countMatches [] _ = []
countMatches (x:xs) idx 
  | x == 'O' || x == '%' = [idx] ++ countMatches xs (idx + 1)
  | otherwise = countMatches xs (idx + 1)


-- prop_checkNonMatchingIndices :: Property
-- prop_checkNonMatchingIndices = forAll (randomStringGuess 8) (\(first, second, guess) -> (nonMatchingIndices 0 first second) == (countElementIndices '%' guess 0))

-- nonMatchingIndices :: Int -> String -> String -> [Int]
-- nonMatchingIndices _ [] [] = []
-- nonMatchingIndices index (x:xs) (y:ys)
--  | x /= y = [index] + nonMatchingIndices (index + 1) xs ys
--  | otherwise = nonMatchingIndices (index + 1) xs ys

-- countElementIndices :: Char -> String -> Int -> [Int]
-- countElementIndices _ [] i = []
-- countElementIndices c (x:xs) i
--   | c == x = [i] + countElementIndices c xs (i + 1)
--   | otherwise = countElementIndices c xs (i + 1)

randomString :: Int -> Gen (String, String)
randomString 0 = return ("", "")
randomString k = do
      letter <- elements ['a' .. 'z']
      secondLetter <- elements ['a' .. 'z']
      (fstWord, sndWord) <- genRandomString (elements [(k - 1)])
      return ([letter] ++ fstWord, [secondLetter] ++ sndWord)

-- randomStringGuess :: Int -> Gen (String, String, String)
-- randomStringGuess 0 = return ("", "", "")
-- randomStringGuess k = do
--       (fst, snd) <- randomString k
--       guess <- guessResult fst snd
--       return (fst, snd, guess)

countMap :: Map Char Int -> Int
countMap map = sum (elems map)

clone :: Int -> String -> String
clone 0 _ = ""
clone num str = str ++ clone (num - 1) str

countNumO :: String -> Int
countNumO "" = 0
countNumO (x : xs)
  | x == 'O' = 1 + countNumO xs
  | otherwise = countNumO xs

countNumOverlap :: String -> String -> Int
countNumOverlap "" "" = 0
countNumOverlap (x : xs) (y : ys)
  | x == y = 1 + countNumOverlap xs ys
  | otherwise = countNumOverlap xs ys

genRandomString :: Gen Int -> Gen (String, String)
genRandomString k = do
  num <- k
  if num == 0
    then return ("", "")
    else do
      letter <- elements ['a' .. 'z']
      secondLetter <- elements ['a' .. 'z']
      (fstWord, sndWord) <- genRandomString (elements [(num - 1)])
      return ([letter] ++ fstWord, [secondLetter] ++ sndWord)


-- replaces chars in result string with 'O' if guess char is in the correct spot
markCorrect :: String -> String -> String -> (String, M.Map Char Int)
markCorrect [] _ _ = ("", M.empty)
markCorrect (g : gs) (w : ws) (r : rs)
  | g == w = ('O' : rs', miss)
  | otherwise = (r : rs', M.insertWith (+) w 1 miss)
  where
    (rs', miss) = markCorrect gs ws rs

-- replaces chars in result string with 'X' if guess char is not in the word
markNotInWord :: String -> String -> String -> String
markNotInWord [] _ _ = ""
markNotInWord (g : gs) word (r : rs)
  | g `elem` word = r : rs'
  | otherwise = 'X' : rs'
  where
    rs' = markNotInWord gs word rs

-- replaces chars in result string with '%' if guess char is in the wrong spot
-- if there are duplicates of some guess char but only one copy of that char in
-- the word, only replace the leftmost char with '%' and the rest with 'X'
markWrongSpot :: String -> M.Map Char Int -> String -> String
markWrongSpot [] _ _ = ""
markWrongSpot (g : gs) miss (r : rs)
  | (r == 'O') || (r == 'X') = r : markWrongSpot gs miss rs
  | otherwise =
      if M.findWithDefault 0 g miss > 0
        then "%" ++ markWrongSpot gs (M.insertWith (+) g (-1) miss) rs
        else "X" ++ markWrongSpot gs miss rs

-- prompts user for guess, repeats if guess is not length 5
-- getGuess :: [String] -> GlobalState -> IO String
-- getGuess dict st = do
--   putStrTUI "guess: "
--   g <- 
--   let guess = applyText T.toUpper g
--   if length guess == 5
--   then if guess `elem` dict
--          then return $ applyText T.toUpper guess
--          else do
--            putStrLnFlush "Guess is not a valid word!"
--            getGuess dict
--   else do
--     putStrLnFlush "Guess must be 5 letters long!"
--     getGuess dict

-- reads message between flags, parses code and word
readAndParseFlagged :: Handle -> IO (String, String)
readAndParseFlagged hdl = do
  msg <- readBetween hdl "-->" "<--"
  return (parseCode msg, parseWord msg)

parseFlagged :: String -> (String, String)
parseFlagged msg = (parseCode noFlags, parseWord noFlags)
  where noFlags = applyText (T.drop 3 . T.dropEnd 3) msg

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
        then return $ applyText (T.drop 3 . T.dropEnd 3) msg
        else readBetween hdl f1 f2

-- reads as many consecutive digits as possible
parseCode :: String -> String
parseCode = applyText (T.takeWhile isDigit . T.strip)

-- drops code, reads as many consecutive letters as possible
parseWord :: String -> String
parseWord = applyText (T.takeWhile isLetter . T.strip . T.dropWhile isDigit . T.strip)

-- automatically packs and unpacks string for Data.Text processing functions
applyText :: (T.Text -> T.Text) -> String -> String
applyText f str = T.unpack $ f $ T.pack str

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
  let (idx, _) = uniformR (0 :: Int, length dict - 1 :: Int) gen
  return $ applyText T.toUpper (dict !! idx)
