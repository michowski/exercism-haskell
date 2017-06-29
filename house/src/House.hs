module House (rhyme) where

import Data.List (intercalate)

verbPerNoun :: [(String, String)]
verbPerNoun =
  [ ("house that Jack built.", "")
  , ("malt", "lay in")
  , ("rat", "ate")
  , ("cat", "killed")
  , ("dog", "worried")
  , ("cow with the crumpled horn", "tossed")
  , ("maiden all forlorn", "milked")
  , ("man all tattered and torn", "kissed")
  , ("priest all shaven and shorn", "married")
  , ("rooster that crowed in the morn", "woke")
  , ("farmer sowing his corn", "kept")
  , ("horse and the hound and the horn", "belonged to")
  ]

rhymePart :: Int -> String
rhymePart n = unlines . map verse $ [0 .. n]
  where
    noun x = fst $ verbPerNoun !! (n - x)
    verb x = snd $ verbPerNoun !! (n - x + 1)
    verse x
      | x == 0 = "This is the " ++ noun x
      | otherwise = "that " ++ verb x ++ " the " ++ noun x

rhyme :: String
rhyme = intercalate "\n" . map rhymePart $ [0 .. length verbPerNoun - 1]
