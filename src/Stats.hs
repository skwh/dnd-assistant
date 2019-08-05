module Stats
    ( 
      Stat (..)
    , readStats
    , checkStat
    , initStat
    , CharacterStats
    , Mod
    ) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad (forM)

data Stat = Strength |
            Athletics |
            Dexterity |
            Acrobatics |
            Stealth |
            SleightOfHand |
            Constitution |
            Intelligence |
            Arcana |
            Nature |
            History |
            Religion |
            Investigation |
            Wisdom |
            AnimalHandling |
            Medicine |
            Perception |
            Survival |
            Insight |
            Charisma |
            Deception |
            Performance |
            Intimidation |
            Persuasion deriving (Eq, Show, Read, Ord, Bounded, Enum)

type Mod = Int
type CharacterStats = M.Map Stat Mod

checkStat :: Stat -> CharacterStats -> Maybe Mod
checkStat = M.lookup

initStat :: Stat -> Mod -> CharacterStats -> CharacterStats
initStat = M.insert

readStats :: IO CharacterStats
readStats = do
  putStrLn "Enter the mod values for each of your character's stats."
  stats <- forM [Strength .. Persuasion] (\a -> do
    putStrLn $ "Enter the mod for " ++ show a ++ "!"
    inp <- getLine
    return (a, fromMaybe 0 $ readMaybe inp)) 
  return $ M.fromList stats

