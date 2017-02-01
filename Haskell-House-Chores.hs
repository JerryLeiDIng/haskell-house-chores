import Data.List.Split
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
    return $ "This is the run\n" ++ (show chores)

parseChores :: String -> [(String, Int, Int)]
parseChores choreLines = map (parseLine . splitOn ",") $ (tail . lines) choreLines
    where parseLine (a:b:c:[]) = (a, read b, read c)

parseHistory :: String -> String
parseHistory x = "TEST"

parseBrothers :: String -> String
parseBrothers x = "TEST"


