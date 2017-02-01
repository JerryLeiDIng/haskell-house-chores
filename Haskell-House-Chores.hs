import Data.List.Split
import qualified Data.Map.Strict as Map
import System.Environment
import System.Exit

import Algorithms.Hungarian as Hungarian

main = getArgs >>= parse >>= putStrLn

parse ["-h"] = help >> exit
parse ["-help"] = help >> exit
parse ["-v"] = validate
parse ["-validate"] = validate
parse ["-r"] = run
parse ["-run"] = run
parse x = usage >> exitWith (ExitFailure 1)

exit = exitWith ExitSuccess

usage = putStrLn "This would be usage instructions"

help = putStrLn "This would be help stuff"

validate = do
    history <- parseHistory <$> readFile "example_chores.txt"
    return "This is the validation"

run = do
    chores <- parseChores <$> readFile "example_chores.txt"
    history <- parseHistory <$> readFile "example_history.txt"
    brothers <- parseBrothers <$> readFile "example_brothers.txt"
    return $ "This is the run\n" ++ (show history)

-- |The parseChores function takes a chore file and parses it to a list of chore tuples
parseChores :: String -> [(String, Int, Int)]
parseChores choreLines = map (parseLine . splitOn ",") $ (tail . lines) choreLines
    where parseLine (a:b:c:[]) = (a, read b, read c)

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

parseHistory :: String -> Map.Map BrotherName BrotherHistory
parseHistory choreLines = makeHistoryMap $ lines choreLines
    -- TODO clean this up using where pattern matching
    where 
        makeHistoryMap :: [String] -> Map.Map BrotherName BrotherHistory
        makeHistoryMap [] = Map.empty
        makeHistoryMap (header:rest) = Map.fromList choreHistory
            where 
                -- headerIndex maps each brother name to an index
                headerIndex :: [(BrotherName, Int)]
                headerIndex = zip (splitOn "\t" header) [0..]
                -- choreParsed is a list of lists of tuples (choreName, choreDifficulty)
                -- Splits each line on \t, then parsing the output into tuples
                -- TODO somehow encode the length of each list in the type system 
                choreParsed :: [[(Chore, Difficulty)]]
                choreParsed = map (parseTuple . splitOn ",") . (splitOn "\t") <$> rest
                    where
                        parseTuple (c:d:[]) = (c, read d :: Int)
                        parseTuple x = error $ "Badly formatted history input: " ++ show x
                -- Each ([String],[Int]) tuple in choreFolded is the (ordered) list of 
                -- chore names and difficulties for the corresponding brother
                choreFolded :: [([Chore], [Difficulty])]
                choreFolded = foldr
                  (\next cum -> collapseIntoList <$> zip cum next)
                  (replicate (length headerIndex) ([],[]))
                  choreParsed
                    where 
                        collapseIntoList ((strs, diffs), (str, diff)) =
                          (str:strs, diff:diffs) 
                -- choreHistory is the association list which maps names to histories
                choreHistory :: [(BrotherName, BrotherHistory)]
                choreHistory = (\((name, index),(chores, diffs)) -> 
                  (name, BrotherHistory {
                    name=name,
                    index=index,
                    previousChores=chores,
                    previousDifficulties=diffs
                  })) <$> zip headerIndex choreFolded


parseBrothers :: String -> String
parseBrothers x = "TEST"


