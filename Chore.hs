module Chore 
( Chore,
makeChore
) where

import qualified Data.Text as Text

data Chore = Chore { choreName :: String
           , numBrothers :: Int
           , difficulty :: Int
           } deriving (Show)

inputRowToList row separator =  Text.splitOn (Text.pack separator) (Text.pack row)

makeChore :: [(String , Int , Int)] -> Chore
makeChore [(a, b, c)] = Chore {choreName = a,
                              numBrothers = b ,
                              difficulty = c}
