module Eval where

import Parse
import Control.Monad.State
import Data.Char

type Tape = ([Int], Int, [Int])

type BF = StateT Tape IO ()

eval :: Token -> BF
eval PInc = modify $ \(left, x, right) -> (x:left, head right, tail right)
eval PDec = modify $ \(left, x, right) -> (tail left, head left, x:right)
eval Inc = modify $ \(left, x, right) -> (left, succ x, right)
eval Dec = modify $ \(left, x, right) -> (left, x - 1, right)
eval Print = do
	(_, x, _) <- get
	liftIO $ putChar $ chr x
eval Input = do
	c <- liftIO getChar
	modify $ \(left, _, right) -> (left, ord c, right)
eval (Loop code) = do
	(left, x, right) <- get
	if x == 0
		then return ()
		else mapM_ eval code >> eval (Loop code)
