module Main where

import           Control.Exception
import           Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches), AllTextSubmatches(getAllTextSubmatches) )


main :: IO ()
main = do
    assert (length (names simpleString) == 4) $ pure ()
    grades <- readFile "./assets/grades.txt"
    assert (length (allStudentsWithB grades) == 16) $ pure ()
    rawLogData <- readFile "./assets/logdata.txt"
    assert (example `elem` (parseLogs rawLogData)) $ pure ()
    putStrLn "All tests passed!"

data Log = MkLog
    {
        host :: String
    ,   userName :: String
    ,   time :: String
    ,   request :: String
    } deriving (Eq, Show)

example :: Log
example = MkLog "146.204.224.152" "feest6811" "21/Jun/2019:15:45:24 -0700" "POST /incentivize HTTP/1.1"


logPattern :: String
logPattern = "^([0-9]{1,3}(\\.[0-9]{1,3}){3}) - ([^ ]+) \\[(.*)\\] \"(.*)\" *"

parseLogs :: String -> [Log]
parseLogs s = map parseLogLine (lines s)
    where
        parseLogLine :: String -> Log
        parseLogLine line = case getAllTextSubmatches (line =~ logPattern :: AllTextSubmatches [] String) of
                                (_full:hostTxt:_octet:userTxt:timeTxt:reqTxt:_) ->
                                    MkLog
                                        { host     = hostTxt
                                        , userName = userTxt
                                        , time     = timeTxt
                                        , request  = reqTxt
                                        }
                                xs -> error ("malformed log line: " ++ (show xs))
{-
import re
def grades():
    grades = ""
    with open ("assets/grades.txt", "r") as file:
        grades = file.read()

    pat = re.compile(r'(?m)^([^:\r\n]+):\s*B(?:\s|$).*')
    return [m.group(1) for m in pat.finditer(s)]
-}
allStudentsWithB :: String -> [String]
allStudentsWithB s = map (takeWhile (/=':')) (getAllTextMatches (s =~ "(^|\\n)(.*:[[:space:]]*B.*)(\\r?\\n|$)" :: AllTextMatches [] String))

{-
Equivalent Python solution:

import re
def names():
    simple_string = """Amy is 5 years old, and her sister Mary is 2 years old. 
    Ruth and Peter, their parents, have 3 kids."""
    pat = re.compile(r"\b[A-Z][A-Za-z]*")

    return pat.findall(simple_string)
-}
names :: String -> [String]
names s = getAllTextMatches (s =~ "\\b[A-Z][A-Za-z]*" :: AllTextMatches [] String)

simpleString :: String
simpleString = "Amy is 5 years old, and her sister Mary is 2 years old. Ruth and Peter, their parents, have 3 kids."
