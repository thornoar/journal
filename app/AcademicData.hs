{-# LANGUAGE BangPatterns #-}
module AcademicData where

import Data.Map (Map, empty, insertWith, foldrWithKey, (!), notMember)
import qualified Data.Map as M (lookup)
import Text.Read (readMaybe)
import Data.List (intercalate)
import System.Directory (doesFileExist)
import Data.Time
import Control.Monad (when)
import qualified System.IO.Strict as S (readFile)
import Data.Maybe (fromMaybe)

type Students = Map Int String
type Problems = Map Int (Float, Int)
type Solutions = Map Int Problems
type Bonus = Map Int Float
type Exam = Map Int Float
type Final = Map Int Int
type Data = (Students, Problems, Solutions, Bonus, Exam, Final)

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
    \n (cost, ddl) !lst -> ("p:" ++ show n ++ "," ++ show cost ++ "," ++ show ddl) : lst
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
    \n prbs !lst -> ("s:" ++ show n ++ ";" ++ intercalate ";" (writeProblems prbs)) : lst
  ) []

readBonus :: [String] -> Bonus
readBonus [] = empty
readBonus (('b':':':str):rest) = case splitBy ',' str of
  [num, val] -> case (readMaybe num :: Maybe Int, readMaybe val :: Maybe Float) of
    (Just n, Just v) -> insert' n v (readBonus rest)
    _ -> readBonus rest
  _ -> readBonus rest
readBonus (_:rest) = readBonus rest

writeBonus :: Bonus -> [String]
writeBonus = foldrWithKey (
    \n v !lst -> ("b:" ++ show n ++ "," ++ show v) : lst
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
    \n g !lst -> ("e:" ++ show n ++ "," ++ show g) : lst
  ) []

readFinal :: [String] -> Final
readFinal [] = empty
readFinal (('f':':':str):rest) = case splitBy ',' str of
  [num, grade] -> case (readMaybe num :: Maybe Int, readMaybe grade :: Maybe Int) of
    (Just n, Just g) -> insert' n g (readFinal rest)
    _ -> readFinal rest
  _ -> readFinal rest
readFinal (_:rest) = readFinal rest

writeFinal :: Final -> [String]
writeFinal = foldrWithKey (
    \n g !lst -> ("f:" ++ show n ++ "," ++ show g) : lst
  ) []

readData :: IO Data
readData = do
  students <- S.readFile "students.data"
  problems <- S.readFile "problems.data"
  solutions <- doesFileExist "solutions-latest.data" >>= \b -> if b
               then S.readFile "solutions-latest.data"
               else pure []
  bonus <- doesFileExist "bonus-latest.data" >>= \b -> if b
               then S.readFile "bonus-latest.data"
               else pure []
  exam <- doesFileExist "exam-latest.data" >>= \b -> if b
          then S.readFile "exam-latest.data"
          else pure []
  final <- doesFileExist "final-latest.data" >>= \b -> if b
          then S.readFile "final-latest.data"
          else pure []
  pure (
      readStudents (lines students),
      readProblems (lines problems),
      readSolutions (lines solutions),
      readBonus (lines bonus),
      readExam (lines exam),
      readFinal (lines final)
    )

pad :: Char -> Int -> String -> String
pad c n str = replicate (n - length str) c ++ str

printDate :: Day -> String
printDate day = pad '0' 2 (show d) ++ "." ++ pad '0' 2 (show m) ++ "." ++ show y
  where (y,m,d) = toGregorian day

printDate' :: Day -> String
printDate' day = show y ++ "-" ++ pad '0' 2 (show m) ++ "-" ++ pad '0' 2 (show d)
  where (y,m,d) = toGregorian day

printTime :: DiffTime -> String
printTime = show . diffTimeToPicoseconds

writeData :: Bool -> Data -> IO ()
writeData extra (_, _, sols, bon, ex, fin) = do
  let solcontents = unlines $ writeSolutions sols
      bonuscontents = unlines $ writeBonus bon
      excontents = unlines $ writeExam ex
      fincontents = unlines $ writeFinal fin
  when extra $ do
    curtime <- getCurrentTime
    let postfix = printDate' (utctDay curtime) ++ "-" ++ printTime (utctDayTime curtime) ++ ".data"
        solfname = "solutions-" ++ postfix
        bonfname = "bonus-" ++ postfix
        exfname = "exam-" ++ postfix
        finfname = "final-" ++ postfix
    writeFile solfname solcontents
    writeFile bonfname bonuscontents
    writeFile exfname excontents
    writeFile finfname fincontents
  writeFile "solutions-latest.data" solcontents
  writeFile "bonus-latest.data" bonuscontents
  writeFile "exam-latest.data" excontents
  writeFile "final-latest.data" fincontents
  putStrLn $ "| " ++ color "32" "Wrote databases to disk."

decayFunction :: Int -> Int -> Float
decayFunction t x
  | x <= t = 1.0
  | otherwise = 0.5

getProblemValue :: Int -> (Float, Int) -> Float
getProblemValue ddl (grade, time) = grade * decayFunction ddl time

getCumulativeProblemGrade :: Problems -> Problems -> Float -> Float
getCumulativeProblemGrade probs solved bonus = 5.0 * (val + bonus) / total
  where
    val = foldrWithKey (
        \n p !acc -> acc + getProblemValue (snd (probs ! n)) p
      ) 0 solved
    total = foldr (
        \ (cost, _) !acc -> acc + cost
      ) 0 probs

getGrade :: Data -> Int -> Int
getGrade (_,prbs,sols,bon,ex,fin) k =
  if notMember k fin
  then getTotalGrade egrade pgrade
  else fin ! k
    where
      egrade = fromMaybe 0 (M.lookup k ex)
      pgrade = getCumulativeProblemGrade prbs (fromMaybe empty $ M.lookup k sols) (fromMaybe 0 $ M.lookup k bon)

getTotalGrade :: Float -> Float -> Int
getTotalGrade eg pg = min 5 . max 2 . round $ 0.5 * eg + 0.6 * pg


color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m" -- ]]

gradeColor :: Float -> Float -> String
gradeColor lim f
  | f/lim < 0.45 = "31"
  | f/lim < 0.85 = "33"
  | otherwise = "32"

printFloat :: Float -> String
printFloat = select . show
  where select :: String -> String
        select ('.':rest) = '.' : take 1 rest
        select (c:rest) = c : select rest
        select [] = []

printGrade :: Float -> Float -> String
printGrade lim f = color (gradeColor lim f) $ if f < 0.1 then "~ 0" else printFloat f

printFinalGrade :: Int -> String
printFinalGrade g
  | g == 2 = color "31" (show g)
  | g == 3 = color "33" (show g)
  | otherwise = color "32" (show g)
