module Parse
(parseChores,
parseBrothers,
parseHistory,
ChoreName,
BrotherName,
Difficulty,
BrotherHistory(..)) where

import Data.List.Split
import Data.List (findIndices)
-- |The parseChores function takes a chore file and parses it to a list of chore tuples
--  Skips the header line
parseChores :: String -> [(String, Int, Int)]
parseChores choreLines = map (parseLine . splitOn ",") $ (tail . lines) choreLines
    where parseLine (a:b:c:[]) = (a, read b, read c)

parseBrothers :: String -> String
parseBrothers x = "TEST"

-- The following datatypes and functions are used for building the chore history
type ChoreName = String
type BrotherName = String
type Difficulty = Int
data BrotherHistory = BrotherHistory {
    name :: BrotherName,
    index :: Int,
    previousChores :: [ChoreName],
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
    choreParsed :: [[(ChoreName, Difficulty)]]
    choreParsed = if and sameLengthAsHeader then out else
        error $ "One or more lines is not the same length as the header\n" ++
        "Check lines " ++ (show $ (+1) <$> findIndices (== False) sameLengthAsHeader)
            where
                parseTuple :: [String] -> (ChoreName, Difficulty)
                parseTuple (c:d:[]) = (c, read d :: Int)
                parseTuple x = error $ "Badly formatted history input: " ++ show x
                out = map (parseTuple . splitOn ",") . tail . splitOn "\t" <$> rest
                -- Error checking: make sure lines are same length
                sameLengthAsHeader = map (== length headerIndex) (length <$> out)
    -- Each ([String],[Int]) tuple in choreFolded is the (ordered) list of 
    -- chore names and difficulties for the corresponding brother
    choreFolded :: [([ChoreName], [Difficulty])]
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


