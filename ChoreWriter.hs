module ChoreWriter (
    writeChoreAssignments 
) where

import Data.List
import qualified System.IO as IO

import Parse

-- | Writes chore assignments to the given file, each brother on a new line
writeChoreAssignments :: String -> [(Parse.BrotherName, Parse.ChoreName)] -> IO [()]
writeChoreAssignments filename assignments = let
    header = "Brother\tChore"
    lines :: [String]
    lines = (header :) $ (\(bro,chore) -> bro ++ "\t" ++ chore) <$> sort assignments   
    in IO.withFile filename IO.WriteMode (sequence . mapM (flip IO.hPutStrLn) lines)
