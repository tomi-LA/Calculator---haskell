{-
  A logical calculator for propositions
  Based on the arithmetic calculator from Lecture 11
  (Chapter 8 of "Programming in Haskell" by Graham Hutton).
-}
module Main where

import Parsing
import Data.Char

--
-- a data type for logical propositions
--
data Prop = Const Bool        -- constants (True/False)
          | Var Char          -- variables
          | And Prop Prop     -- conjunction
          | Or Prop Prop      -- disjunction
          | Not Prop          -- negation
          deriving Show

-- | a parser for propositions
-- Grammar rules:
--
-- prop     ::= term propCont
-- propCont ::= '|' term propCont | epsilon
--
-- term     ::= factor termCont
-- termCont ::= '&' factor termCont | epsilon
--
-- factor   ::= '!' factor | name | constant | '(' prop ')'
-- constant ::= 'T' | 'F'

prop :: Parser Prop
prop = do t <- term
          propCont t

propCont :: Prop -> Parser Prop
propCont acc = do char '|'
                  t <- term
                  propCont (Or acc t)
               <|> return acc

term :: Parser Prop
term = do f <- factor
          termCont f

termCont :: Prop -> Parser Prop
termCont acc = do char '&'
                  f <- factor
                  termCont (And acc f)
               <|> return acc

factor :: Parser Prop
factor = do char '!'
            f <- factor
            return (Not f)
         <|>
         name
         <|>
         constant
         <|>
         do char '('
            p <- prop
            char ')'
            return p

-- | variable names: single lowercase letters
name :: Parser Prop
name = do c <- satisfy isLower
          return (Var c)

-- | boolean constants: 'T' and 'F'
constant :: Parser Prop
constant = do char 'T'
              return (Const True)
           <|>
           do char 'F'
              return (Const False)

----------------------------------------------------------------

main :: IO ()
main
  = do txt <- getContents
       calculator (lines txt)

-- | read-parse-print loop (temporary: shows the parse tree)
calculator :: [String] -> IO ()
calculator []     = return ()
calculator (l:ls) = do putStrLn (showParse l)
                       calculator ls

-- | parse a single proposition and show the resulting tree
showParse :: String -> String
showParse txt
  = case parse prop txt of
      [ (p, "") ] -> show p
      _           -> "parse error; try again"
