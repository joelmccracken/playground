module Chapter24 where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative


stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof

one' = one >> stop

oneTwo = char '1' >> char '2' >> eof

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParse' :: Parser () -> IO ()
testParse' p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn $ '\n' : s

p123 s =
  let
    pc = string "123" <|> string "12" <|> string "1"
  in parseString pc mempty s


string' :: String -> Parser String
string' [] =
  return ""

string' (x:xs) = do
  x' <- char x
  xs' <- string' xs
  return $ x' : xs'

main :: IO ()
main = do

  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse' one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse' oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

  pNL "p123 1"
  print $ p123 "1"

  pNL "p123 12"
  print $ p123 "12"

  pNL "p123 123"
  print $ p123 "123"

  pNL "string'"
  print $ parseString (string' "1233") mempty "123"
