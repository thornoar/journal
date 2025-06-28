module AcademicData where

import Data.Map (Map, empty, insertWith, foldrWithKey, (!))
import Text.Read (readMaybe)
import Data.List (intercalate)
import System.Directory (doesFileExist)
import Data.Time
import Control.Monad (when)
import qualified System.IO.Strict as S (readFile)

type Students = Map Int String
type Problems = Map Int (Float, Int)
type Solutions = Map Int Problems
type Exam = Map Int Float
type Data = (Students, Problems, Solutions, Exam)

insert' :: Ord k => k -> a -> Map k a -> Map k a
insert' = insertWith (\_ b -> b)

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy ch (ch':rest)
  | ch == ch' = [] : splitBy ch rest
  | otherwise = case splitBy ch rest of
      [] -> [[ch']]
      w : ws -> (ch' : w) : ws

readStudents :: [String] -> Students
readStudents [] = empty
readStudents (line:rest) = case line of
  's':':':str -> case splitBy ',' str of
    [num, name] -> case (readMaybe num :: Maybe Int) of
      Nothing -> readStudents rest
      Just n -> insert' n name (readStudents rest)
    _ -> readStudents rest
  _ -> readStudents rest

readProblems :: [String] -> Problems
readProblems [] = empty
readProblems (('p':':':str):rest) = case splitBy ',' str of
  [num, cost, expi] -> case (readMaybe num :: Maybe Int, readMaybe cost :: Maybe Float, readMaybe expi :: Maybe Int) of
    (Just n, Just c, Just e) -> insert' n (c, e) $ readProblems rest
    _ -> readProblems rest
  _ -> readProblems rest
readProblems (_:rest) = readProblems rest

writeProblems :: Problems -> [String]
writeProblems = foldrWithKey (
    \n (cost, ddl) lst -> ("p:" ++ show n ++ "," ++ show cost ++ "," ++ show ddl) : lst
  ) []

readSolutions :: [String] -> Solutions
readSolutions [] = empty
readSolutions (('s':':':str):rest) = case splitBy ';' str of
  num:probs -> case (readMaybe num :: Maybe Int) of
    Nothing -> readSolutions rest
    Just n -> insert' n (readProblems probs) (readSolutions rest)
  _ -> readSolutions rest
readSolutions (_:rest) = readSolutions rest

writeSolutions :: Solutions -> [String]
writeSolutions = foldrWithKey (
    \n prbs lst -> ("s:" ++ show n ++ ";" ++ intercalate ";" (writeProblems prbs)) : lst
  ) []

readExam :: [String] -> Exam
readExam [] = empty
readExam (('e':':':str):rest) = case splitBy ',' str of
  [num, grade] -> case (readMaybe num :: Maybe Int, readMaybe grade :: Maybe Float) of
    (Just n, Just g) -> insert' n g (readExam rest)
    _ -> readExam rest
  _ -> readExam rest
readExam (_:rest) = readExam rest

writeExam :: Exam -> [String]
writeExam = foldrWithKey (
    \n g lst -> ("e:" ++ show n ++ "," ++ show g) : lst
  ) []

readData :: IO Data
readData = do
  students <- S.readFile "students.data"
  problems <- S.readFile "problems.data"
  solutions <- doesFileExist "solutions-latest.data" >>= \b -> if b
               then S.readFile "solutions-latest.data"
               else pure []
  exam <- doesFileExist "exam-latest.data" >>= \b -> if b
          then S.readFile "exam-latest.data"
          else pure []
  pure (
      readStudents (lines students),
      readProblems (lines problems),
      readSolutions (lines solutions),
      readExam (lines exam)
    )

pad :: Char -> Int -> String -> String
pad c n str = replicate (n - length str) c ++ str

printDate :: Day -> String
printDate day = pad '0' 2 (show d) ++ "." ++ pad '0' 2 (show m) ++ "." ++ show y
  where (y,m,d) = toGregorian day

printTime :: DiffTime -> String
printTime = show . diffTimeToPicoseconds

writeData :: Bool -> (Solutions, Exam) -> IO ()
writeData extra (sols, ex) = do
  let solcontents = unlines $ writeSolutions sols
      excontents = unlines $ writeExam ex
  when extra $ do
    curtime <- getCurrentTime
    let postfix = printDate (utctDay curtime) ++ "-" ++ printTime (utctDayTime curtime) ++ ".data"
        solfname = "solutions-" ++ postfix
        exfname = "exam-" ++ postfix
    writeFile solfname solcontents
    writeFile exfname excontents
  writeFile "solutions-latest.data" solcontents
  writeFile "exam-latest.data" excontents
  putStrLn $ "| " ++ color "32" "Wrote databases to disk."



decayFunction :: Int -> Float -> Float
decayFunction t x = if x <= fromIntegral t then 1 - (x / fromIntegral t)^(2 :: Int) / 2 else 0.5

getProblemValue :: Int -> (Float, Int) -> Float
getProblemValue ddl (grade, time) = grade * decayFunction ddl (fromIntegral time)

getCumulativeProblemGrade :: Problems -> Problems -> Float
getCumulativeProblemGrade probs solved = 5.0 * val / total
  where
    val = foldrWithKey (
        \n p acc -> acc + getProblemValue (snd (probs ! n)) p
      ) 0 solved
    total = foldr (
        \ (cost, _) acc -> acc + cost
      ) 0 probs

getTotalGrade :: Float -> Float -> Float
getTotalGrade eg pg = min 5.0 $ 0.5 * eg + 0.6 * pg


color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m" -- ]]

gradeColor :: Float -> Float -> String
gradeColor lim f
  | f/lim < 0.45 = "31"
  | f/lim < 0.85 = "33"
  | otherwise = "32"

printGrade :: Float -> Float -> String
printGrade lim f = color (gradeColor lim f) $ if f < 0.1 then "~ 0" else take 3 (show f)
