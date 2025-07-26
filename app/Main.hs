{-# LANGUAGE BangPatterns #-}
module Main where

import System.Console.Haskeline
import System.Directory
import Data.Map (empty, insertWith, foldrWithKey, (!), notMember, mapWithKey, Map)
import qualified Data.Map as M (lookup)
import qualified System.Random as R
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Data.Time

import AcademicData
import AI
import System.Environment (lookupEnv)
import Control.Monad (forM_, unless)
import Data.List (sortBy, intercalate)

type Action a = InputT IO a

startDate :: Day
startDate = fromGregorian 2025 7 19
-- startDate = fromGregorian 2025 6 25

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just "/home/ramak/.journal-history",
  autoAddHistory = True
}

putError :: String -> IO ()
putError msg = do
  putStrLn ""
  putStrLn $ color "1;31" "Error: " ++ msg
  spec <- lookupEnv "SPECIALISATION"
  if spec /= Just "hyprland-powersave"
  then createError msg
  else putStrLn ""

outputError :: String -> Action ()
outputError = liftIO . putError

exit :: Data -> Action ()
exit d = do
  liftIO $ writeData False d
  outputStrLn $ "| " ++ color "32" "Quitting..."
  outputStrLn "| "
  pure ()

printStudentProfile :: Data -> Int -> Action Int
printStudentProfile (stds, prbs, sols, bon, ex, fin) n = do
  if notMember n stds
  then outputError "No such student." >> return 0
  else do
    let name = fromMaybe "no such student" (M.lookup n stds)
    outputStrLn $ "| " ++ color "33" (pad '0' 2 $ show n) ++ " --- " ++ color "35" name
    outputStr "| || "
    let egrade = fromMaybe 0 (M.lookup n ex)
    outputStr (printGrade 5.0 egrade)
    outputStr " | "
    let solved = fromMaybe empty (M.lookup n sols)
        (total, val, pgrade) = getCumulativeProblemGrade prbs solved (fromMaybe 0 $ M.lookup n bon)
    outputStr $ printGrade total val ++ "/" ++ color "32" (show total)
    let bonusgrade = fromMaybe 0 (M.lookup n bon)
    outputStr $ " + " ++ color "32" (if bonusgrade < 0.1 then "0.0" else printFloat bonusgrade)
    outputStr $ " -> " ++ printGrade 5.0 pgrade ++ " | "
    let totalGrade = fromMaybe (getTotalGrade egrade pgrade) (M.lookup n fin)
    if notMember n fin
    then do
      outputStrLn $ "-> " ++ printFinalGrade totalGrade ++ "?"
    else do
      outputStrLn $ "Total grade: " ++ printFinalGrade totalGrade
    return totalGrade

printSolvedProblems :: Data -> Int -> Action ()
printSolvedProblems (_, prbs, sols, _, _, _) n = do
  let solved = fromMaybe empty (M.lookup n sols)
  foldrWithKey (\k (gr, time) !action ->
      case M.lookup k prbs of
        Nothing -> action
        Just (cost, ddl) -> do
          outputStr "| ||     | "
          outputStrLn $
            color "33" (show k) ++ " (grade " ++
            printGrade cost gr ++ "/" ++ color "32" (show cost) ++
            " at day " ++ color (if time <= ddl then "32" else "31") (show time) ++ "/" ++ color "32" (show ddl) ++ ") "
          action
    ) (pure ()) solved

listRanking :: Data -> Action ()
listRanking d@(stds, _, _, _, _, _) = do
  let
      grades :: Map Int Int
      grades = mapWithKey (\k _ -> getGrade d k) stds
      group :: [Int] -> ([Int], [Int], [Int], [Int])
      group [] = ([],[],[],[])
      group (x:xs) =
        let (g1,g2,g3,g4) = group xs
            f = fromMaybe 0 $ M.lookup x grades
            res
              | f > 4 = (x:g1,g2,g3,g4)
              | f > 3 = (g1,x:g2,g3,g4)
              | f > 2 = (g1,g2,x:g3,g4)
              | otherwise = (g1,g2,g3,x:g4)
            in res
      (i1,i2,i3,i4) = group $ sortBy
        (\j1 j2 -> compare (M.lookup j2 grades) (M.lookup j1 grades))
        $ foldrWithKey (\k _ !acc -> k : acc) [] stds
  unless (null i1) $ do
    outputStrLn $ "| || " ++ color "32" "Grade > 4.0"
    forM_ i1 $ \i ->
      printStudentProfile d i
    outputStrLn "|"
  unless (null i2) $ do
    outputStrLn $ "| || " ++ color "33" "Grade > 3.0"
    forM_ i2 $ \i ->
      printStudentProfile d i
    outputStrLn "|"
  unless (null i3) $ do
    outputStrLn $ "| || " ++ color "31" "Grade > 2.0"
    forM_ i3 $ \i ->
      printStudentProfile d i
    outputStrLn "|"
  unless (null i4) $ do
    outputStrLn $ "| || " ++ color "31" "The worthless scum"
    forM_ i4 $ \i ->
      printStudentProfile d i

listAll :: Data -> Action ()
listAll d@(stds, _, _, _, _, _) = foldrWithKey (
    \n _ !action ->
      printStudentProfile d n >> printSolvedProblems d n >> action
  ) (pure ()) stds

listProblem :: Int -> (Float, Int) -> Action ()
listProblem n (cost, ddl) = do
  let expday = addDays (toInteger ddl) startDate
  curday <- liftIO $ fmap utctDay getCurrentTime
  outputStrLn $
    "| " ++ color "33" (pad '0' 2 $ show n) ++
    ". Cost: " ++ color "35" (show cost) ++
    ", Expires on " ++ color "35" (printDate expday) ++
    ", in " ++ color "35" (show (diffDays expday curday)) ++ " days."

listProblems :: Problems -> Action ()
listProblems = foldrWithKey (
    \n val !action -> do
      listProblem n val
      action
  ) (pure ())

listStudents :: Students -> Action ()
listStudents = foldrWithKey (
    \n name !action -> outputStrLn ("| " ++ color "33" (pad '0' 2 $ show n) ++ ". " ++ color "35" name) >> action
  ) (pure ())

stonks :: Ordering -> String
stonks LT = color "32" "(stonks)"
stonks GT = color "31" "(not stonks)"
stonks EQ = "(no grade change)"

rainbowColor :: String -> String
rainbowColor = intercalate "." . zipWith color ["35","34","34","34","34"] . splitBy '.'

printHelp :: Action ()
printHelp = forM_ (map ("| " ++) [
    rainbowColor "l a" ++ "         - list all students and their grades."
  , rainbowColor "l r" ++ "         - print the student ranking table."
  , rainbowColor "l p" ++ "         - list all problems and their parameters."
  , rainbowColor "l s" ++ "         - give a simple list of all students."
  , rainbowColor "l ps" ++ "        - list problems and students in sequence."
  , rainbowColor "l @n" ++ "        - print info about student by key @n."
  , rainbowColor "s @n @p @g" ++ "  - add a solution: student @n solved @p with grade @g."
  , rainbowColor "e @n @g" ++ "     - add an exam grade: student @n received exam grade @g."
  , rainbowColor "w" ++ "           - write solution/exam databases to disk, with a time code."
  , rainbowColor "?" ++ "           - print this help message."
  ]) outputStrLn

printProblemProfile :: Data -> Int -> Action ()
printProblemProfile d@(stds, prbs, sols, _, _, _) n = do
  if notMember n prbs
  then outputError "No such problem."
  else do
    listProblem n (prbs ! n)
    outputStrLn "|"
    outputStrLn $ "| " ++ color "32" "Submitted:"
    outputStrLn "|"
    foldrWithKey (
        \k _ !action ->
          if notMember n (fromMaybe empty $ M.lookup k sols)
          then action
          else printStudentProfile d k >> action
      ) (pure ()) stds
    outputStrLn "|"
    outputStrLn $ "| " ++ color "31" "Did not submit:"
    foldrWithKey (
        \k _ !action ->
          if notMember n (fromMaybe empty $ M.lookup k sols)
          then printStudentProfile d k >> action
          else action
      ) (pure ()) stds

addSolution :: Data -> Int -> Int -> Float -> Action Data
addSolution d@(stds, prbs, sols, bon, ex, fin) n p g = do
  if notMember n stds || notMember p prbs || g <= 0 || g > fst (prbs ! p)
  then outputError "Invalid argument values for setting a solution." >> return d
  else do
    time <- fmap (fromInteger . (`diffDays` startDate) . utctDay) (liftIO getCurrentTime)
    let newmap = insertWith const p (g, time) $ fromMaybe empty (M.lookup n sols)
        newdata = (
            stds, prbs,
            insertWith const n newmap sols,
            bon,
            ex,
            fin
          )
    outputStrLn $ "| " ++ color "32" "Record succesfully updated."
    outputStrLn "|"
    let oldGrade = getGrade d n
    newGrade <- printStudentProfile newdata n
    printSolvedProblems newdata n
    outputStrLn "|"
    outputStrLn $ "| " ++ stonks (compare oldGrade newGrade)
    return newdata

addBonus :: Data -> Int -> Float -> Action Data
addBonus d@(stds, prbs, sols, bon, ex, fin) n v = do
  if notMember n stds
  then outputError "Invalid arguments for setting bonus points." >> return d
  else do
    let newdata = (
            stds, prbs, sols,
            insertWith const n (v + fromMaybe 0 (M.lookup n bon)) bon,
            ex, fin
          )
    outputStrLn $ "| " ++ color "32" "Record succesfully updated."
    outputStrLn "|"
    let oldGrade = getGrade d n
    newGrade <- printStudentProfile newdata n
    outputStrLn "|"
    outputStrLn $ "| " ++ stonks (compare oldGrade newGrade)
    return newdata

addExam :: Data -> Int -> Float -> Action Data
addExam d@(stds, prbs, sols, bon, ex, fin) n g = do
  if notMember n stds || g < 0.0 || g > 5.0
  then outputError "Invalid arguments for setting exam grades." >> return d
  else do
    let newdata = (
            stds, prbs, sols, bon,
            insertWith const n g ex, fin
          )
    outputStrLn $ "| " ++ color "32" "Record succesfully updated."
    outputStrLn "|"
    let oldGrade = getGrade d n
    newGrade <- printStudentProfile newdata n
    outputStrLn "|"
    outputStrLn $ "| " ++ stonks (compare oldGrade newGrade)
    return newdata

handleCommand :: Data -> String -> Action ()
handleCommand d "" = prompt d
handleCommand _ "force quit" = pure ()
handleCommand d "exit" = exit d
handleCommand d "quit" = exit d
handleCommand d@(stds, prbs, _, _, _, _) args = case splitBy ' ' args of
  ["l", "a"] -> listAll d >> prompt d
  ["l", "r"] -> listRanking d >> prompt d
  ["l", "p"] -> listProblems prbs >> prompt d
  ["l", "s"] -> listStudents stds >> prompt d
  ["l", "ps"] -> listProblems prbs >> outputStrLn "|" >> listStudents stds >> prompt d
  ["l", num] -> case (readMaybe num :: Maybe Int) of
    Just n -> printStudentProfile d n >> printSolvedProblems d n >> prompt d
    Nothing -> outputError "Invalid argument format for listing." >> prompt d
  ["l", "p", num] -> case (readMaybe num :: Maybe Int) of
    Just n -> printProblemProfile d n >> prompt d
    Nothing -> outputError "Invalid argument format for listing." >> prompt d
  ["s", num, prob, grade] -> case (readMaybe num :: Maybe Int, readMaybe prob :: Maybe Int, readMaybe grade :: Maybe Float) of
    (Just n, Just p, Just g) -> addSolution d n p g >>= prompt
    _ -> outputError "Invalid argument format." >> prompt d
  ["b", num, val] -> case (readMaybe num :: Maybe Int, readMaybe val :: Maybe Float) of
    (Just n, Just v) -> addBonus d n v >>= prompt
    _ -> outputError "Invalid argument format for setting bonus points." >> prompt d
  ["e", num, grade] -> case (readMaybe num :: Maybe Int, readMaybe grade :: Maybe Float) of
    (Just n, Just g) -> addExam d n g >>= prompt
    _ -> outputError "Invalid argument format for setting exam grades." >> prompt d
  ["w"] -> liftIO (writeData True d) >> prompt d
  ["r", low, up] -> case (readMaybe low :: Maybe Int, readMaybe up :: Maybe Int) of
    (Just l, Just u) -> randomPrompt l u >> prompt d
    _ -> outputError "Invalid argument format for entering random number generator." >> prompt d
  ["?"] -> printHelp >> prompt d
  _ -> outputError ("Unrecognized command: " ++ args ++ ".") >> prompt d

prompt :: Data -> Action ()
prompt d = handleInterrupt (prompt d) $ do
  outputStrLn "|"
  minput <- getInputLine ("[" ++ color "33" "journal" ++ "] : ")
  case minput of
    Nothing -> outputStrLn "|" >> exit d
    Just "" -> prompt d
    Just input -> outputStrLn "|" >> handleCommand d input

randomPrompt :: Int -> Int -> Action ()
randomPrompt low up = do
  minput <- getInputLine ("[" ++ color "35" "randomizer" ++ "] : ")
  case minput of
    Nothing -> pure ()
    Just _ -> do
      num <- liftIO (R.randomIO :: IO Int)
      outputStrLn $ "| Random number is: " ++ color "34" (show $ low + mod num (up - low + 1))
      randomPrompt low up

main :: IO ()
main = do
  studentsExist <- doesFileExist "journal/students.data"
  problemsExist <- doesFileExist "journal/problems.data"
  if not studentsExist || not problemsExist
  then putError "Could not read student and problem data files."
  else do
    setCurrentDirectory "journal"
    d <- readData
    runInputT settings $ withInterrupt (prompt d)
