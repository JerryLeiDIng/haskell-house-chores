import qualified Data.Text as Text

data Chore = Chore { choreName :: String
                   , numBrothers :: Int
                   , difficulty :: Int
                   } deriving (Show)

inputRowToList row separator =  Text.splitOn (Text.pack separator) (Text.pack row)

listToChore [String a, String b, String c] = Chore {choreName = a,
                                                              numBrothers = read b :: Int,
                                                              difficulty = read c :: Int}
