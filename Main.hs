module Main where

import System.Console.Haskeline
import System.Environment
import System.IO
import Control.Applicative
import Parse
import Evaluate
import DataStructure


-- Error message or result number.
type EvalResult = Either String Number


-- Return wrapped list of bare results if all inputs are Right.
-- Otherwise, returns the first Left error message.
allRight :: [EvalResult] -> Either String [Number]
allRight = foldr (liftA2 (:)) (Right [])


-- If second argument is Left, prefix the error message
-- with the first argument. Otherwise, pass-through.
prefixError :: String -> Either String a -> Either String a
prefixError prefix (Left errMsg) = Left (prefix ++ errMsg)
prefixError _      rightResult   = rightResult


tryParseEval :: String -> EvalResult
tryParseEval expStr =
    parse expStr >>= (prefixError "Evaluation error: " . eval)


-- Convert result to string or return error message.
showParseEval :: String -> String
showParseEval expStr =
    case tryParseEval expStr of
      Right result  -> show result
      Left errorMsg -> errorMsg


-- If standard input is a terminal, display a REPL.
-- Otherwise, just parse, eval, and show each line.
main :: IO ()
main = do
    isTerminal <- hIsTerminalDevice stdin
    args <- getArgs
    if isTerminal || ("--repl" `elem` args)
        then runInputT defaultSettings superRepl
        else getContents >>= mapM_ (putStrLn . showParseEval) . lines


-- Parses and evaluates given line.
-- For debugging, show the parses in addition to the evaluation result.
-- If returns Nothing, it's time to quit.
handleReplLine :: String -> Maybe String
handleReplLine line =
    if line `elem` ["q", ":q", "quit", ":quit", "exit"] then
        Nothing
    else
        let unparsedStr = case parse line of
                Right parsed -> show parsed ++ "\n"
                _            -> ""
        in Just $
            (show . parse $ line) ++ "\n" ++ unparsedStr ++ showParseEval line


-- Use if Haskeline doesn't work.
simpleRepl :: IO ()
simpleRepl = do
    putStr "> "
    hFlush stdout
    line <- getLine
    case handleReplLine line of
        Nothing        -> return ()    -- Time to quit.
        Just resultStr -> do
            putStrLn resultStr
            simpleRepl


-- Uses Haskeline so that you can hit the up arrow.
superRepl :: InputT IO ()
superRepl = do
    maybeLine <- getInputLine "> "
    case maybeLine of
        Nothing   -> return ()    -- Time to quit.
        Just line ->
            case handleReplLine line of
                Nothing        -> return ()    -- Time to quit.
                Just resultStr -> do
                    outputStrLn resultStr
                    superRepl
