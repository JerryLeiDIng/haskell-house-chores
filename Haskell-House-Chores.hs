import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import System.Directory
import System.Environment
import System.Exit
import qualified System.IO as IO

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

-- | Constants and globals
historyFile = "example_history.txt"
brothersFile = "example_brothers.txt"
choresFile = "example_chores.txt"


-- | Run the actual program
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
parse ["-t"] = test
parse x = usage >> exitWith (ExitFailure 1)

exit = exitWith ExitSuccess


--TODO delete me after testing is done
test = do
    return "This is a test command that should be deleted in the final version"

-- | Cleans the history file and creates a blank one with the
--   column headers corresponding to brothers in brothersFile 
--   Prompts the user for confirmation and backs up the history file
--   TODO annotate the backup file with the EXACT time and not just the date
clean = do
    putStrLn "Warning!"
    putStrLn $ "This action will delete the entire history file at " ++ historyFile
    putStr "Continue? (y/n):"
    IO.hFlush IO.stdout
    continue <- getLine
    date <- getDateString
    if continue == "y" 
      then
        let backUpHistory = historyFile ++ "." ++ date ++ ".bak" in
        copyFile historyFile backUpHistory >>
        putStrLn ("Old history file backed up at " ++ backUpHistory) >>
        (Parse.parseBrothers <$> readFile brothersFile) >>=
        (\bros -> writeFile historyFile $ intercalate "\t" ("Date":bros)) >>
        return "New history file generated!"
      else
        return "Aborting!"
        

-- | TODO help should print out information about all usage flags and modes
help = putStrLn "This would be help stuff"

-- | Assigns chores using default assignment filename
run = do
    string <- getDateString
    let filename = "chore_assignments_" ++ string ++ ".txt"
    runWith filename

-- | Assigns chores using specified assignment filename
--   TODO create flag for validation usage which prints the assignments
--   but does not write to file or update the history
runWith filename = do
    putStrLn $ "Creating assignments. Output will be written to " ++ filename
    chores <- Parse.parseChores <$> readFile choresFile
    history <- Parse.parseHistory <$> readFile historyFile
    let slots :: [(Parse.ChoreName, Parse.Difficulty)]
        slots = makeChoreSlots chores $ length history
        assignments :: [(Parse.BrotherName, Parse.ChoreName)]
        assignments = setUpHungarianMatrix slots history
    handle <- IO.openFile filename IO.WriteMode 
    ops <- writeChoreAssignments assignments handle
    IO.hClose handle
    putStrLn $ (show $ length ops - 1) ++ " chore assignments written"
    createNewWeekHistory (sortBy (\(b1,_) (b2,_) -> compare b1 b2) assignments) historyFile
    return $ "History file updated at " ++ historyFile


usage = putStrLn "Invalid command\nFor help, run `haskell-house-chores -h`"

-- TODO validate should check the number of brothers/chores
-- List of brothers who will be missing, brothers w/ penalty chores
-- Prompt to continue
-- Prints out tentative chore assignments but DOES NOT save them
validate = do
    return "This is the validation"


-- | This function converts the list of (Chore, NumBrothers, Difficulty)
--   tuples into the final list of (Chore, Diff) slots
--   Throws error if more chore slots then finalLength
--   Pads the list to length finalLength if shorter
makeChoreSlots :: [(Parse.ChoreName, Int, Parse.Difficulty)] -> Int -> [(Parse.ChoreName, Parse.Difficulty)]
makeChoreSlots chores finalLength = let
    rawSlots = concatMap (\(chore, n, diff) -> replicate n (chore, diff)) chores
    in if length rawSlots > finalLength
        then error "The number of chore slots is greater than the number of brothers!"
        else rawSlots ++ (replicate (finalLength - (length rawSlots)) ("No chore", 0))


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





