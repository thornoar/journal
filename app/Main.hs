module Main where

import System.Console.Haskeline
import System.Directory
import Data.Map (empty, insertWith, foldrWithKey, (!), notMember)
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Data.Time

import AcademicData
import AI

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
  -- putStrLn $ "| " ++ color "1;31" "Error: "
  putStrLn ""
  putStrLn $ color "1;31" "Error: " ++ msg
  createError msg

outputError :: String -> Action ()
outputError = liftIO . putError

exit :: (Solutions, Exam) -> Action ()
exit d = do
  liftIO $ writeData False d
  outputStrLn $ "| " ++ color "32" "Quitting..."
  outputStrLn "| "
  return ()

printFullStudentInfo :: Data -> Int -> Action ()
printFullStudentInfo (stds, prbs, sols, ex) n = do
  let name = stds ! n
  outputStrLn $ "| " ++ color "33" (show n) ++ " --- " ++ color "35" name
  outputStr "| | "
  let egrade = fromMaybe 0 (M.lookup n ex)
  outputStr (printGrade 5.0 egrade)
  outputStr " | "
  let solved = fromMaybe empty (M.lookup n sols)
      pgrade = getCumulativeProblemGrade prbs solved
  outputStr (printGrade 5.0 pgrade)
  outputStrLn $ " | -> " ++ printGrade 5.0 (getTotalGrade egrade pgrade)
  foldrWithKey (\k (gr, time) action ->
      case M.lookup k prbs of
        Nothing -> action
        Just (cost, ddl) -> do
          outputStr "| |     | "
          outputStrLn $
            color "33" (show k) ++ " (grade " ++
            printGrade cost gr ++ "/" ++ color "32" (show cost) ++
            " at day " ++ color "33" (show time) ++ "/" ++ color "32" (show ddl) ++ ") "
          action
    ) (return ()) solved

listAll :: Data -> Action ()
listAll d = foldrWithKey (
    \n _ action -> printFullStudentInfo d n >> action
  ) (return ()) (qfst d)

listProblems :: Problems -> Action ()
listProblems = liftIO . foldrWithKey (
    \n (cost, ddl) action -> do
      let expday = addDays (toInteger ddl) startDate
      curday <- fmap utctDay getCurrentTime
      putStrLn $
        "| " ++ color "33" (show n) ++
        ". Cost: " ++ color "35" (show cost) ++
        ", Expires on " ++ color "35" (printDate expday) ++
        ", in " ++ color "35" (show (diffDays expday curday)) ++ " days."
      action
  ) (return ())

listStudents :: Students -> Action ()
listStudents = foldrWithKey (
    \n name action -> outputStrLn ("| " ++ color "33" (show n) ++ " --- " ++ color "35" name) >> action
  ) (return ())

handleCommand :: Data -> String -> Action ()
handleCommand d "" = prompt d
handleCommand _ "force quit" = return ()
handleCommand (_, _, sols, ex) "exit" = exit (sols, ex)
handleCommand (_, _, sols, ex) "quit" = exit (sols, ex)
handleCommand (stds, prbs, sols, ex) args = case splitBy ' ' args of
  ["l", "a"] -> do
    listAll (stds, prbs, sols, ex)
    prompt (stds, prbs, sols, ex)
  ["l", "p"] -> do
    listProblems prbs
    prompt (stds, prbs, sols, ex)
  ["l", "s"] -> do
    listStudents stds
    prompt (stds, prbs, sols, ex)
  ["l", "ps"] -> do
    listProblems prbs
    outputStrLn "|"
    listStudents stds
    prompt (stds, prbs, sols, ex)
  ["l", num] -> case (readMaybe num :: Maybe Int) of
    Just n -> do
      printFullStudentInfo (stds, prbs, sols, ex) n
      prompt (stds, prbs, sols, ex)
    Nothing -> do
      outputError "Invalid argument format for listing."
      prompt (stds, prbs, sols, ex)
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
        printFullStudentInfo newdata n
        prompt newdata
    _ -> do
      outputError "Invalid argument format."
      prompt (stds, prbs, sols, ex)
  ["e", num, grade] -> case (readMaybe num :: Maybe Int, readMaybe grade :: Maybe Float) of
    (Just n, Just g) -> do
      if notMember n stds || g <= 1.0 || g > 5.0
      then do
        outputError "Invalid arguments for setting exam grades."
        prompt (stds, prbs, sols, ex)
      else do
        let newdata = (
                stds, prbs, sols,
                insertWith const n g ex
              )
        outputStrLn $ "| " ++ color "32" "Record succesfully updated."
        printFullStudentInfo newdata n
        prompt newdata
    _ -> do
      outputError "Invalid argument format for setting exam grades."
      prompt (stds, prbs, sols, ex)
  ["w"] -> liftIO (writeData True (sols, ex)) >> prompt (stds, prbs, sols, ex)
  _ -> do
    outputError $ "Unrecognized command: " ++ args ++ "."
    prompt (stds, prbs, sols, ex)

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
