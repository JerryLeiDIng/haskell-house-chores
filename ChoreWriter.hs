module ChoreWriter (
    getDate,
    getDateString,
    writeChoreAssignments,
    createNewWeekHistory,
    updateNewWeekHistory
) where

import Data.List
import Data.List.Split
import Data.Time.Calendar
import Data.Time.Clock
import qualified System.IO as IO

import Parse (ChoreName, BrotherName, Difficulty, parseLatestHistory)


-- TODO think about how this interface should look.
--      When a new week of chores is generated a new line is appended to the history
--      This line is (Chore,-1) for each brother (all incomplete)
--      When the houseman updates the chore completion status through the website it will call the haskell function to update the brother(s) responsible
--      One problem: how to extract original difficulty? Solution: back up working chore and brother list files in tmp directory!

-- | Utility methods to obtain the date
getDate :: IO (Integer, Int, Int) -- :: (year, month, day)
getDate = getCurrentTime >>= return . toGregorian . utctDay
getDateString :: IO String -- :: in (YYYY-mm-dd) format
getDateString = do
    let lpad r = if length r == 1 then '0':r else r
    (year, month, day) <- getDate
    return $ intercalate "-" [lpad.show $ year, lpad.show $ month, lpad.show $ day] 


-- | Writes chore assignments to the given file, each brother on a new line
writeChoreAssignments :: [(BrotherName, ChoreName)] -> IO.Handle -> IO [()]
writeChoreAssignments assignments handle = let
    header = "Brother\tChore"
    lines :: [String]
    lines = (header :) $ (\(bro,chore) -> bro ++ "\t" ++ chore) <$> sort assignments   
    in mapM (IO.hPutStrLn handle) lines


-- | Creates a new line in the history file for which all brothers are incomplete
--      NOTE columns are written in order given.
--      be sure brother order is the same as the existing order in the history file
--      Otherwise an error will be raised
createNewWeekHistory :: [(BrotherName, ChoreName)] -> String -> IO ()
createNewWeekHistory assignments filename = do
    historyContents <- readFile filename
    dateFormatted <- getDateString
    let headerNames = splitOn "\t" $ head $ lines historyContents
        assignmentNames = map fst assignments
        newTokens = dateFormatted : map (flip (++) ",-1" . show . fst) assignments
        newLine = intercalate "\t" newTokens
    if headerNames /= assignmentNames
        then error $ "Fatal: History column header and provided names did not match!" 
        else appendFile filename newLine

-- | Updates the latest week's history to set the status of the(chore, brother) pairs
--      If any pair is invalid, will throw error!
--      Note that Difficulty will be -1 for incomplete chores
updateNewWeekHistory :: [(BrotherName, ChoreName, Difficulty)] -> String -> IO ()
updateNewWeekHistory updates filename = do
    return ()
    


-- | Appends the new set of chores to the history file
--   Writes a backup version of the current history
--   Note that columns are written IN ORDER THEY ARE GIVEN
--   Be sure order is the same as the existing order in the history file
