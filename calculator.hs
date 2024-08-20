#!/usr/bin/env runhaskell

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (isPrefixOf)
import System.IO (hFlush, isEOF, stdout)

data Token
  = IdentifierToken
  | OperatorToken
  | ParenthesisToken
  deriving (Show, Eq)

isIdentifierToken c
  | isAlphaNum c || c == '_' = True
  | otherwise = False

isOperatorToken :: Char -> Bool
isOperatorToken c
  | c `elem` ['=', '+', '-', '*', '/', '^'] = True
  | otherwise = False

isParenthesisToken :: Char -> Bool
isParenthesisToken c
  | c `elem` ['(', ')'] = True
  | otherwise = False

tokenizeLine :: String -> Maybe [(Char, Token)]
tokenizeLine "" = Just []
tokenizeLine (x : xs)
  | isIdentifierToken x = ((x, IdentifierToken) :) <$> tokenizeLine xs
  | isParenthesisToken x = ((x, ParenthesisToken) :) <$> tokenizeLine xs
  | isOperatorToken x = ((x, OperatorToken) :) <$> tokenizeLine xs
  | otherwise = Nothing

-- validateLine :: [(Char, Token)] -> (Bool, [String])
-- validateLine l = (True, ["Success!"])

eval :: String -> String
eval l = show $ tokenizeLine l

evalLoop :: IO ()
evalLoop =
  do
    putStr "> "
    hFlush stdout
    done <- isEOF
    if done
      then pure ()
      else do
        line <- getLine
        putStrLn $ eval line
        evalLoop

main :: IO ()
main = evalLoop
