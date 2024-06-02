module Main where

import Parse
import Eval

import System.Environment 
import Control.Monad.State
import Text.Parsec

run :: String -> IO ()
run str = do
	case parse parseCode "" str of
		Right tokens -> void $ execStateT (mapM_ eval tokens) (repeat 0, 0, repeat 0)
		Left err -> print err

main :: IO ()
main = do
	args <- getArgs
	run $ head args
