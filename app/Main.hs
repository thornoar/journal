{-# LANGUAGE BangPatterns #-}
module Main where

import System.Console.Haskeline
import System.Directory
import Data.Map (empty, insertWith, foldrWithKey, (!), notMember, mapWithKey, Map)
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Data.Time

import AcademicData
import AI
import System.Environment (lookupEnv)
import Control.Monad (forM_, unless)
import Data.List (sortBy)

type Action a = InputT IO a

startDate :: Day
-- startDate = fromGregorian 2025 7 19
startDate = fromGregorian 2025 6 25

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

exit :: (Solutions, Exam) -> Action ()
exit d = do
  liftIO $ writeData False d
  outputStrLn $ "| " ++ color "32" "Quitting..."
  outputStrLn "| "
  pure ()

printStudentProfile :: Data -> Int -> Action Float
printStudentProfile (stds, prbs, sols, ex) n = do
  let name = stds ! n
  outputStrLn $ "| " ++ color "33" (pad '0' 2 $ show n) ++ " --- " ++ color "35" name
  outputStr "| || "
  let egrade = fromMaybe 0 (M.lookup n ex)
  outputStr (printGrade 5.0 egrade)
  outputStr " | "
  let solved = fromMaybe empty (M.lookup n sols)
      pgrade = getCumulativeProblemGrade prbs solved
  outputStr (printGrade 5.0 pgrade)
  let totalGrade = getTotalGrade egrade pgrade
  outputStrLn $ " | -> " ++ printGrade 5.0 totalGrade
  return totalGrade

printSolvedProblems :: Data -> Int -> Action ()
printSolvedProblems (_, prbs, sols, _) n = do
  let solved = fromMaybe empty (M.lookup n sols)
  foldrWithKey (\k (gr, time) !action ->
      case M.lookup k prbs of
        Nothing -> action
        Just (cost, ddl) -> do
          outputStr "| ||     | "
          outputStrLn $
            color "33" (show k) ++ " (grade " ++
            printGrade cost gr ++ "/" ++ color "32" (show cost) ++
            " at day " ++ color "33" (show time) ++ "/" ++ color "32" (show ddl) ++ ") "
          action
    ) (pure ()) solved

listRanking :: Data -> Action ()
listRanking d@(stds, _, _, _) = do
  let
      grades :: Map Int Float
      grades = mapWithKey (\k _ -> getGrade d k) stds
      group :: [Int] -> ([Int], [Int], [Int], [Int])
      group [] = ([],[],[],[])
      group (x:xs) =
        let (g1,g2,g3,g4) = group xs
            f = fromMaybe 0.0 $ M.lookup x grades
            res
              | f > 4.0 = (x:g1,g2,g3,g4)
              | f > 3.0 = (g1,x:g2,g3,g4)
              | f > 2.0 = (g1,g2,x:g3,g4)
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
listAll d@(stds, _, _, _) = foldrWithKey (
    \n _ !action ->
      printStudentProfile d n >> printSolvedProblems d n >> action
  ) (pure ()) stds

listProblems :: Problems -> Action ()
listProblems = liftIO . foldrWithKey (
    \n (cost, ddl) !action -> do
      let expday = addDays (toInteger ddl) startDate
      curday <- fmap utctDay getCurrentTime
      putStrLn $
        "| " ++ color "33" (pad '0' 2 $ show n) ++
        ". Cost: " ++ color "35" (show cost) ++
        ", Expires on " ++ color "35" (printDate expday) ++
        ", in " ++ color "35" (show (diffDays expday curday)) ++ " days."
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

handleCommand :: Data -> String -> Action ()
handleCommand d "" = prompt d
handleCommand _ "force quit" = pure ()
handleCommand (_, _, sols, ex) "exit" = exit (sols, ex)
handleCommand (_, _, sols, ex) "quit" = exit (sols, ex)
handleCommand d@(stds, prbs, sols, ex) args = case splitBy '.' args of
  ["l", "a"] -> listAll d >> prompt d
  ["l", "r"] -> listRanking d >> prompt d
  ["l", "p"] -> listProblems prbs >> prompt d
  ["l", "s"] -> listStudents stds >> prompt d
  ["l", "ps"] -> do
    listProblems prbs
    outputStrLn "|"
    listStudents stds
    prompt d
  ["l", num] -> case (readMaybe num :: Maybe Int) of
    Just n -> do
      _ <- printStudentProfile d n
      printSolvedProblems d n
      prompt d
    Nothing -> do
      outputError "Invalid argument format for listing."
      prompt d
  ["s", num, prob, grade] -> case (readMaybe num :: Maybe Int, readMaybe prob :: Maybe Int, readMaybe grade :: Maybe Float) of
    (Just n, Just p, Just g) -> do
      if notMember n stds || notMember p prbs || g <= 0 || g > fst (prbs ! p)
      then do
        outputError "Invalid argument values for setting a solution."
        prompt (stds, prbs, sols, ex)
      else do
        time <- fmap (fromInteger . (`diffDays` startDate) . utctDay) (liftIO getCurrentTime)
        let newmap = insertWith const p (g, time) $ fromMaybe empty (M.lookup n sols)
            newdata = (
                stds, prbs,
                insertWith const n newmap sols,
                ex
              )
        outputStrLn $ "| " ++ color "32" "Record succesfully updated."
        outputStrLn "|"
        let oldGrade = getGrade d n
        newGrade <- printStudentProfile newdata n
        printSolvedProblems newdata n
        outputStrLn "|"
        outputStrLn $ "| " ++ stonks (compare oldGrade newGrade)
        prompt newdata
    _ -> do
      outputError "Invalid argument format."
      prompt d
  ["e", num, grade] -> case (readMaybe num :: Maybe Int, readMaybe grade :: Maybe Float) of
    (Just n, Just g) -> do
      if notMember n stds || g < 0.0 || g > 5.0
      then do
        outputError "Invalid arguments for setting exam grades."
        prompt (stds, prbs, sols, ex)
      else do
        let newdata = (
                stds, prbs, sols,
                insertWith const n g ex
              )
        outputStrLn $ "| " ++ color "32" "Record succesfully updated."
        outputStrLn "|"
        let oldGrade = getGrade d n
        newGrade <- printStudentProfile newdata n
        outputStrLn "|"
        outputStrLn $ "| " ++ stonks (compare oldGrade newGrade)
        prompt newdata
    _ -> do
      outputError "Invalid argument format for setting exam grades."
      prompt d
  ["w"] -> liftIO (writeData True (sols, ex)) >> prompt (stds, prbs, sols, ex)
  _ -> do
    outputError $ "Unrecognized command: " ++ args ++ "."
    prompt d

prompt :: Data -> Action ()
prompt d = handleInterrupt (prompt d) $ do
  outputStrLn "|"
  minput <- getInputLine ("[" ++ color "33" "journal" ++ "] : ")
  case minput of
    Nothing -> outputStrLn "|" >> case d of
      (_, _, sols, ex) -> exit (sols, ex)
    Just "" -> prompt d
    Just input -> outputStrLn "|" >> handleCommand d input

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
