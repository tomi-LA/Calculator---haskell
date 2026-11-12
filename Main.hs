-- Logical calculator: classifies propositions using truth tables
module Main where

import Parsing
import Data.Char

-- propositions
data Prop = Const Bool
          | Var Char
          | And Prop Prop
          | Or Prop Prop
          | Not Prop
          deriving Show

-- parser (& has higher precedence than |)
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

-- variables are single lowercase letters
name :: Parser Prop
name = do c <- satisfy isLower
          return (Var c)

constant :: Parser Prop
constant = do char 'T'
              return (Const True)
           <|>
           do char 'F'
              return (Const False)

-- truth tables
type Subst = [(Char, Bool)]

find :: Char -> Subst -> Bool
find x ((y, v):s) | x == y    = v
                  | otherwise = find x s
find _ []                     = False

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x)   = find x s
eval s (And p q) = eval s p && eval s q
eval s (Or p q)  = eval s p || eval s q
eval s (Not p)   = not (eval s p)

-- variables of a proposition (with duplicates)
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x)   = [x]
vars (And p q) = vars p ++ vars q
vars (Or p q)  = vars p ++ vars q
vars (Not p)   = vars p

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = [b : bs | b <- [False, True], bs <- bools (n-1)]

-- all possible assignments to the variables
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

truthTable :: Prop -> [(Subst, Bool)]
truthTable p = [(s, eval s p) | s <- substs p]

classify :: Prop -> String
classify p
  | and results = "TAUTOLOGY"
  | or results  = "CONTINGENT"
  | otherwise   = "CONTRADICTION"
  where results = map snd (truthTable p)

-- read-classify-print loop
main :: IO ()
main
  = do txt <- getContents
       calculator (lines txt)

calculator :: [String] -> IO ()
calculator []     = return ()
calculator (l:ls) = do putStrLn (evaluate l)
                       calculator ls

evaluate :: String -> String
evaluate txt
  = case parse prop txt of
      [ (p, "") ] -> classify p
      _           -> "parse error; try again"
