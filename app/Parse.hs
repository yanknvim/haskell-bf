module Parse where

import Text.Parsec
import Text.Parsec.String

data Token = PInc | PDec | Inc | Dec | Print | Input | Loop [Token] deriving Show

parsePInc :: Parser Token
parsePInc = char '>' >> return PInc

parsePDec :: Parser Token
parsePDec = char '<' >> return PDec

parseInc :: Parser Token
parseInc = char '+' >> return Inc

parseDec :: Parser Token
parseDec = char '-' >> return Dec

parsePrint :: Parser Token
parsePrint = char '.' >> return Print

parseInput :: Parser Token
parseInput = char ',' >> return Input

parseLoop :: Parser Token
parseLoop = Loop <$> between (char '[') (char ']') parseCode

parseCode :: Parser [Token]
parseCode = 
	many1 (parsePInc
		<|>  parsePDec
		<|>  parseInc
		<|>  parseDec
		<|>  parsePrint
		<|>  parseInput
		<|>  parseLoop)

