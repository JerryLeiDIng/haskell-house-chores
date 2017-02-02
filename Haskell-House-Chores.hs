import Data.List (findIndices)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Time.Calendar
import Data.Time.Clock
import System.Environment
import System.Exit

import qualified Algorithms.Hungarian as Hungarian

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
    chores <- parseChores <$> readFile "example_chores.txt"
    history <- parseHistory <$> readFile "example_history.txt"
    brothers <- parseBrothers <$> readFile "example_brothers.txt"
    return $ show history



-- |The parseChores function takes a chore file and parses it to a list of chore tuples
--  Skips the header line
parseChores :: String -> [(String, Int, Int)]
parseChores choreLines = map (parseLine . splitOn ",") $ (tail . lines) choreLines
    where parseLine (a:b:c:[]) = (a, read b, read c)

parseBrothers :: String -> String
parseBrothers x = "TEST"

-- The following datatypes and functions are used for building the chore history
type Chore = String
type BrotherName = String
type Difficulty = Int
data BrotherHistory = BrotherHistory {
    name :: BrotherName,
    index :: Int,
    previousChores :: [Chore],
    previousDifficulties :: [Difficulty]
} deriving (Eq, Show)

-- | parseHistory parses the history file into an association list of brothers to their
--   chore histories. It uses the header line to count the number of brothers in total
--   NOTE: The first column is assumed to contain dates. It is IGNORED during parsing
parseHistory :: String -> [(BrotherName, BrotherHistory)]
parseHistory historyFile = let
    -- header has header line, rest is list of rest of lines
    (header:rest) = lines historyFile
    -- headerIndex maps each brother name to an index
    headerIndex :: [(BrotherName, Int)]
    headerIndex = zip ((tail . splitOn "\t") header) [0..]
    -- choreParsed is a list of lists of tuples (choreName, choreDifficulty)
    -- Splits each line on \t, then parsing the output into tuples
    -- TODO somehow encode the length of each list in the type system 
    -- Or at very least CHECK that the list of each line is the same!
    choreParsed :: [[(Chore, Difficulty)]]
    choreParsed = if and sameLengthAsHeader then out else
        error $ "One or more lines is not the same length as the header\n" ++
        "Check lines " ++ (show $ (+1) <$> findIndices (== False) sameLengthAsHeader)
            where
                parseTuple :: [String] -> (Chore, Difficulty)
                parseTuple (c:d:[]) = (c, read d :: Int)
                parseTuple x = error $ "Badly formatted history input: " ++ show x
                out = map (parseTuple . splitOn ",") . tail . splitOn "\t" <$> rest
                -- Error checking: make sure lines are same length
                sameLengthAsHeader = map (== length headerIndex) (length <$> out)
    -- Each ([String],[Int]) tuple in choreFolded is the (ordered) list of 
    -- chore names and difficulties for the corresponding brother
    choreFolded :: [([Chore], [Difficulty])]
    choreFolded = foldr
      (zipWith (\(str, diff) (strs, diffs) -> (str:strs, diff:diffs)))
      (replicate (length headerIndex) ([],[]))
      choreParsed
    -- Create the association list which maps names to histories
    in zipWith
      (\(name, index) (chores, diffs) -> 
        (name, BrotherHistory {
          name=name,
          index=index,
          previousChores=chores,
          previousDifficulties=diffs
        })) headerIndex choreFolded


-- | This function runs the actual Hungarian algorithm
setUpHungarianMatrix :: [(Chore, Difficulty)] -> [(BrotherName, BrotherHistory)] -> [(BrotherName, Chore)]
setUpHungarianMatrix chores brothers = let
    makeCostFn :: Floating a => BrotherHistory -> (Chore, Difficulty) -> a
    makeCostFn bhist = 
        (\ (chore,diff) -> ((fromIntegral $ sum $ previousDifficulties bhist) + 
            (if elem chore $ previousChores bhist then 0.1 else 0 ))**2)

    brotherCostFns :: Floating a => [(Chore, Difficulty) -> a]
    brotherCostFns = (\(_, bhist) -> makeCostFn bhist) <$> brothers

    costMatrix :: Floating a => [a]
    costMatrix = brotherCostFns <*> chores

    hungarianResult :: [(Int, Int)]
    hungarianResult = if length brothers == length chores
        then fst $ Hungarian.hungarian costMatrix (length chores) (length brothers)
        else error "Number of brothers and chores not the same!"

    in (\(choreIdx, broIdx) -> (fst $ brothers !! broIdx, fst $ chores !! choreIdx)) <$> hungarianResult





