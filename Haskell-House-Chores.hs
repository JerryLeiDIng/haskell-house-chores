import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Time.Calendar
import Data.Time.Clock
import System.Environment
import System.Exit

import qualified Algorithms.Hungarian as Hungarian

import Parse
import ChoreWriter

-- NOTE In the future, this file should only handle argument parsing and 
--      function dispatch. The majority of the work should be split off into
--      separate modules
-- Tentative module distribution
--      * Module for clearing and resetting the history for a new semester
--      * Module for generating a new week's chore assignments
--      * Module for setting chore statuses to complete(?)

-- Utility method to obtain the date
date :: IO (Integer, Int, Int) -- :: (year, month, day)
date = getCurrentTime >>= return . toGregorian . utctDay

main = getArgs >>= parse >>= putStrLn

-- | Command line argument parsing
parse ["-c"] = clean
parse ["-clean"] = clean
parse ["-h"] = help >> exit
parse ["-help"] = help >> exit
parse ["-r"] = run
parse ["-run"] = run 
parse ("-r":filename:[]) = runWith filename
parse ("-run":filename:[]) = runWith filename
parse ["-v"] = validate
parse ["-validate"] = validate
-- TODO get rid of me after testing is done!
parse ["-t"] = test >> exit
parse x = usage >> exitWith (ExitFailure 1)

exit = exitWith ExitSuccess


--TODO delete me after testing is done
test = do
    let choreList :: [(Parse.BrotherName, Parse.ChoreName)]
        choreList = [("A1", "Little Kitchen"), ("B2", "Big Kitchen"), ("A1", "QChore")]
    writeChoreAssignments "test_assignments.txt" choreList


-- TODO clean should clear the history
-- But first PROMPT the user and backup the history file to be safe
clean = do
    return "This will clear and regenerate the history file"

-- TODO help should print out information about all usage flags and modes
help = putStrLn "This would be help stuff"

run = do
    (year, month, day) <- date
    let filename = "chore_assignments_" ++ (show year) ++ "-" ++ (show month) ++ "-" ++ (show day) ++ ".txt"
    runWith filename

runWith filename = do
    putStrLn $ "Creating assignments. Output will be written to " ++ filename
    chores <- Parse.parseChores <$> readFile "example_chores.txt"
    history <- Parse.parseHistory <$> readFile "example_history.txt"
    brothers <- Parse.parseBrothers <$> readFile "example_brothers.txt"
    return $ show history

usage = putStrLn "Invalid command\nFor help, run `haskell-house-chores -h`"

-- TODO validate should check the number of brothers/chores
-- List of brothers who will be missing, brothers w/ penalty chores
-- Prompt to continue
-- Prints out tentative chore assignments but DOES NOT save them
validate = do
    return "This is the validation"



-- | This function runs the actual Hungarian algorithm
setUpHungarianMatrix :: [(Parse.ChoreName, Parse.Difficulty)] -> [(Parse.BrotherName, Parse.BrotherHistory)] -> [(Parse.BrotherName, Parse.ChoreName)]
setUpHungarianMatrix chores brothers = let
    makeCostFn :: Floating a => Parse.BrotherHistory -> (Parse.ChoreName, Parse.Difficulty) -> a
    makeCostFn bhist = 
        (\ (chore,diff) -> ((fromIntegral $ sum $ previousDifficulties bhist) + 
            (if elem chore $ previousChores bhist then 0.1 else 0 ))**2)

    brotherCostFns :: Floating a => [(Parse.ChoreName, Parse.Difficulty) -> a]
    brotherCostFns = (\(_, bhist) -> makeCostFn bhist) <$> brothers

    costMatrix :: Floating a => [a]
    costMatrix = brotherCostFns <*> chores

    hungarianResult :: [(Int, Int)]
    hungarianResult = if length brothers == length chores
        then fst $ Hungarian.hungarian costMatrix (length chores) (length brothers)
        else error "Number of brothers and chores not the same!"

    in (\(choreIdx, broIdx) -> (fst $ brothers !! broIdx, fst $ chores !! choreIdx)) <$> hungarianResult





