import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Time.Calendar
import Data.Time.Clock
import System.Environment
import System.Exit

import qualified Algorithms.Hungarian as Hungarian

import Parse

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
parse ["-h"] = help >> exit
parse ["-help"] = help >> exit
parse ["-v"] = validate
parse ["-validate"] = validate
parse ["-r"] = run
parse ["-run"] = run 
parse ("-r":filename:[]) = runWith filename
parse ("-run":filename:[]) = runWith filename
parse x = usage >> exitWith (ExitFailure 1)

exit = exitWith ExitSuccess

usage = putStrLn "This would be usage instructions"

help = putStrLn "This would be help stuff"

validate = do
    history <- parseHistory <$> readFile "example_chores.txt"
    return "This is the validation"

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





