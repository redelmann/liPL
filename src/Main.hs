
-- | This module is the main program for the
-- Simpl evaluator and type checker.
module Main(main) where

import LiPL
import TypeCheck
import Parser
import Evaluator
import System.Environment (getArgs)
import Data.List (partition)
import Control.Monad (when)


-- | Prints the result of evaluating an expression.
evaluate :: Bool -> Bool -> Expression -> IO ()
evaluate displayCode displayConstraints expr = case inferCheck expr of
	Left err -> putStrLn err
	Right (cs, _, expr) -> do
		when displayConstraints $ putStrLn $ show cs ++ "\n"
		when displayCode $ putStrLn $ show expr ++ "\n"
		putStrLn $ case run expr of
			Left err -> err
			Right v -> show v

-- | Evaluates Simpl programs passed as arguments.
main :: IO ()
main = do
	(args, ls) <- fmap (partition (\x -> take 1 x == "-")) getArgs
	let displayCode = "-d" `elem` args
	let displayConstraints = "-c" `elem` args
	sequence $ map (runProgram displayCode displayConstraints) ls
	return ()
	where
		runProgram :: Bool -> Bool -> String -> IO ()
		runProgram displayCode displayConstraints file = do
			parsed <- getProgram file
			case parsed of 
				Left e -> putStrLn e
				Right expr -> evaluate displayCode displayConstraints expr