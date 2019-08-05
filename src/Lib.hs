{-# LANGUAGE TupleSections #-}

module Lib
  ( startRepl
  )
where

import           Stats                          ( CharacterStats
                                                , readStats
                                                , checkStat
                                                , Stat (..)
                                                )
import           DndFile                        ( saveCharacter
                                                , readCharacter
                                                )
import           Dice

import Text.Read (readMaybe)
import Data.Char (toUpper, toLower)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Map.Strict               as M
import           System.Console.Readline        ( readline
                                                , addHistory
                                                )
import           Control.Monad                  ( when )

makeTemplate :: Int -> Int -> String
makeTemplate r m =
  "Rolled " ++ show r ++ " + " ++ show m ++ " = " ++ show (r + m)

roll :: [String] -> CharacterStats -> IO (String, CharacterStats)
roll []  x = programError x
roll [a] x = do
  let message = checkStat (read $ toCapitalCase a) x
  if isNothing message
    then return ("No character has been loaded!", x)
    else do result <- doRoll 20
            return (makeTemplate result $ fromJust message, x)
roll [a, b]      x = (, x) . show <$> doRolls (read b, read a)
roll [a, b, "+"] x = (, x) . show . sum <$> doRolls (read b, read a)
roll _           x = programError x

new :: [String] -> CharacterStats -> IO (String, CharacterStats)
new _ _ = do
  xs <- readStats
  putStrLn "Please enter a name: "
  inp <- readline "> "
  let name = fromJust inp
  (smsg, _) <- save [name] xs
  putStrLn smsg
  return ("Got it!", xs)

save :: [String] -> CharacterStats -> IO (String, CharacterStats)
save []  x = programError x
save [a] x = do
  saveCharacter x a
  return ("Saved " ++ a, x)
save _ x = programError x

load :: [String] -> CharacterStats -> IO (String, CharacterStats)
load []  x = programError x
load [a] _ = ("Loaded " ++ a, ) <$> readCharacter a
load _   x = programError x

showStat :: [String] -> CharacterStats -> IO (String, CharacterStats)
showStat _ xs = return $ (, xs) . show $ M.toList xs

check :: [String] -> CharacterStats -> IO (String, CharacterStats)
check [a] xs = do
  let stat = fromMaybe Intelligence $ readMaybe $ toCapitalCase a
      message = checkStat stat xs
  if isNothing message
  then return ("No character has been loaded!", xs)
  else return (show $ fromJust message, xs)
check _   xs = programError xs

which :: [String] -> CharacterStats -> IO (String, CharacterStats)
which = undefined

dispatch
  :: [(String, [String] -> CharacterStats -> IO (String, CharacterStats))]
dispatch =
  [ ("roll"     , roll)
  , ("new"      , new)
  , ("showStat" , showStat)
  , ("checkStat", check)
  , ("load"     , load)
  , ("save"     , save)
  , ("which"    , which)
  ]

unknownCommand :: [String] -> CharacterStats -> IO (String, CharacterStats)
unknownCommand _ x = return ("unknown command", x)

programError :: CharacterStats -> IO (String, CharacterStats)
programError cs = return ("Oops, I didn't quite get that.", cs)

readCommand :: String -> (String, [String])
readCommand s = let is = words s in (head is, tail is)

toCapitalCase :: String -> String
toCapitalCase [] = []
toCapitalCase (x:xs) = toUpper x : map toLower xs

startRepl :: IO ()
startRepl = repl M.empty

repl :: CharacterStats -> IO ()
repl s = do
  Just line <- readline "> "
  when (not $ null line) $ do
    addHistory line
    let (cmd, args) = readCommand line
        action = fromMaybe unknownCommand $ lookup cmd dispatch
    (result, xs) <- action args s
    putStrLn result
    repl xs
