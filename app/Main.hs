module Main where

import Control.Applicative
import Language.LambdaCalculus
import Options

data MainArgs = MainArgs {outNum :: Bool, inNum :: Bool, λargs :: String}
  deriving (Eq, Ord, Show)

instance Options MainArgs where
  defineOptions = MainArgs
    <$> simpleOption "outnum" False
        "Display the output as a number"
    <*> simpleOption "innum" False
        "Treat arguments as numbers"
    <*> simpleOption "args" []
        "Additional arguments (space separated)"


main :: IO ()
main = runCommand (\opts args -> do
  case args of
    [filename] -> do
      expr <- parseFile filename
      let expr' = foldl App expr (map (if inNum opts then (fromNum.read) else parseTerm) (words (λargs opts)))
      case outNum opts of
        False -> putStrLn (pp $ nf expr')
        True -> print (toNum $ nf expr')
    _ -> error "Bad arguments")
