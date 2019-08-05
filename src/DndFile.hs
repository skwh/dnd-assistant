module DndFile
  ( saveCharacter
  , readCharacter
  )
where

import           Stats                          ( CharacterStats )
import qualified Data.Map.Strict               as M

extension :: String 
extension = ".dnd.chr"

characterStringToMap :: String -> CharacterStats
characterStringToMap = M.fromList . read

saveCharacter :: CharacterStats -> String -> IO ()
saveCharacter mp n = writeFile (n ++ extension) $ show $ M.toList mp

readCharacter :: String -> IO CharacterStats
readCharacter n = characterStringToMap <$> readFile (n ++ extension)
