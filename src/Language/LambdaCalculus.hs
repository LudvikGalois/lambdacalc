{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.LambdaCalculus where

-- I just wanted α-equiv for free, but since the untyped λ-calculus
-- is the tutorial for Bound, it somehow feels a lot less satisfying
import Bound
import Control.Monad (ap, join, void)
import Data.Char
import Data.Deriving
import Data.Functor.Classes
import Text.Parsec

data Expr a = Abs (Scope () Expr a) | App (Expr a) (Expr a) | Var a
  deriving (Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = Var
  (<*>) = ap

instance Monad Expr where
  return = Var
  Var v >>= f = f v
  App e1 e2 >>= f = App (e1 >>= f) (e2 >>= f)
  Abs e >>= f = Abs (e >>>= f)

deriveEq1 ''Expr
deriveOrd1 ''Expr
deriveRead1 ''Expr
deriveShow1 ''Expr

instance Eq a => Eq (Expr a) where (==) = eq1
instance Ord a => Ord (Expr a) where compare = compare1
instance Show a => Show (Expr a) where showsPrec = showsPrec1
instance Read a => Read (Expr a) where readsPrec = readsPrec1
  
lambda :: Eq a => a -> Expr a -> Expr a
lambda v e = Abs (abstract1 v e)

whnf :: Expr a -> Expr a
whnf x@Var{} = x
whnf x@Abs{} = x
whnf (App m n) = case whnf m of
  Abs m' -> whnf (instantiate1 n m')
  m' -> App m' n

nf :: Expr a -> Expr a
nf (App m n) = case whnf m of
  Abs m' -> nf (instantiate1 n m')
  m' -> (App (nf m') (nf n))
nf (Abs m) = Abs $ toScope $ nf $ fromScope m
nf x@Var{} = x

lzero = lambda "s" (lambda "z" (Var "z"))
lone = lambda "s" (lambda "z" (App (Var "s") (Var "z")))
ltwo = lambda "s" (lambda "z" (App (Var "s") (App (Var "s") (Var "z"))))

lsucc = lambda "n" $ lambda "s" $ lambda "z" $ (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))

displayTerm :: [String] -> Expr String -> String
displayTerm _ (Var v) = v
displayTerm c (App m@Abs{} n@Var{}) = concat ["(",displayTerm c m,") ", displayTerm c n]
displayTerm c (App m n@Var{}) = concat [displayTerm c m, displayTerm c n]
displayTerm c (App m@Abs{} n) = concat ["(",displayTerm c m,")", " (",displayTerm c n,")"]
displayTerm c (App m n) = concat [displayTerm c m,"(",displayTerm c n,")"]
displayTerm (c:cs) (Abs m) = 'λ': c ++ (abbrev $ '.': (displayTerm cs $ instantiate (const (return c)) m))
  where abbrev ('.':'λ':s) = s -- Normal shorthand
        abbrev s = s

pp :: Expr String -> String
pp = displayTerm names
  where names = (map return $ ['a'..'z']) ++ (map (++ "'") names)

data LG = VarL String | AbsL String LG | AppL LG LG | Macro String
  deriving (Eq, Ord, Show)

exprParser :: Stream s m Char => ParsecT s u m LG
exprParser = ((try lambdaParser)
         <|> (try appParser)
         <|> (VarL <$> try varParser)
         <|> (try macroParser)
         <|> (try $ parens exprParser)) <* spaces'

exprParser' :: Stream s m Char => ParsecT s u m LG
exprParser' = ((try lambdaParser)
          <|> (try macroParser)
          <|> (try $ parens exprParser)
          <|> (VarL <$> try varParser)) <* spaces'

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (char '(' <* spaces') (char ')')

varParser :: Stream s m Char => ParsecT s u m String
varParser = (:) <$> satisfy (\c -> isLetter c && c /= 'λ') <*> many (char '\'') 

macroParser :: Stream s m Char => ParsecT s u m LG
macroParser = Macro <$> ((char '#') >> manyTill (satisfy (\c -> isLetter c && c /= 'λ')) (try (lookAhead (space <|> oneOf "()#"))))

lambdaParser :: Stream s m Char => ParsecT s u m LG
lambdaParser = do
  oneOf "λ\\"
  spaces'
  vars <- many (try varParser)
  spaces'
  char '.'
  spaces'
  expr <- exprParser
  return $ foldr AbsL expr vars

appParser :: Stream s m Char => ParsecT s u m LG
appParser = do
  x <- try exprParser'
  xs <- many1 (try exprParser')
  return $ foldl1 AppL (x:xs)

macroLineParser :: Stream s m Char => ParsecT s u m (String, LG)
macroLineParser = do
  name <- many1 letter
  spaces'
  string "::="
  spaces'
  expr <- exprParser
  return (name,expr)

fileParser :: Stream s m Char => ParsecT s u m ([(String, LG)], LG)
fileParser = do
  macros <- many ((try macroLineParser) <* (many1 newline))
  spaces
  expr <- exprParser
  spaces
  eof
  return (macros, expr)

-- Parse an expression, folding in macros
parseFile :: FilePath -> IO (Expr String)
parseFile path = do
  fileContents <- readFile path
  case parse fileParser path fileContents of
    Left err -> error (show err)
    Right (macros, expr) -> return $ fromLG $ applyMacros macros expr

parseTerm :: String -> Expr String
parseTerm s =  case parse exprParser "" s of
  Left err -> error (show err)
  Right e -> (fromLG e)

applyMacros :: [(String, LG)] -> LG -> LG
applyMacros [] x = x
applyMacros (m:ms) x = applyMacros [(n,replace m v) | (n,v) <- ms] (replace m x)

replace :: (String, LG) -> LG -> LG
replace (m,v) (Macro m') | m == m' = v
replace x (AppL m n) = AppL (replace x m) (replace x n)
replace x (AbsL v m) = AbsL v (replace x m)
replace _ m = m

spaces' :: Stream s m Char => ParsecT s u m ()
spaces' = void $ many (oneOf " \t") 

fromLG :: LG -> Expr String
fromLG (VarL v) = Var v
fromLG (AppL m n) = App (fromLG m) (fromLG n)
fromLG (AbsL v e) = lambda v (fromLG e)
fromLG (Macro s) = error $ "Undefined macro: " ++ s

toNum :: Expr String -> Int
toNum (Abs x) = case instantiate1 (Var "s") x of
  Abs y -> toNum' $ instantiate1 (Var "z") y 
  y -> error $ "Not a church numeral " ++ pp y
toNum x = error $ "Not a church numeral" ++ pp x

toNum' :: Expr String -> Int
toNum' (Var "z") = 0
toNum' (App (Var "s") n) = succ (toNum' n)
toNum' x = error $ "Not a church numeral " ++ pp x

fromNum :: Int -> Expr String
fromNum n = lambda "s" $ lambda "z" $ foldr App (Var "z") (replicate n (Var "s"))
