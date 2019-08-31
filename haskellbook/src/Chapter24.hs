{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Chapter24 where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative
import Data.Ratio ((%))
import Data.Maybe (fromMaybe)
import Text.RawString.QQ
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Test.Hspec
import Text.Read (readMaybe)
import Data.Char (isAlphaNum)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')

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

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

-- exercise: unit of success
yourFuncHere :: Parser Integer
yourFuncHere = integer <* eof
-- *Chapter24 Text.Trifecta> parseString yourFuncHere mempty "123"
-- Success 123



-- exercise: try try

parseIntegerOrFrac :: Parser (Either Integer Rational)
parseIntegerOrFrac =
  try (Right <$> parseFraction) <|> (Left <$> decimal)

-- ini parsing

headerEx :: ByteString
headerEx = "[blah]"

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
  char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


commentEx :: ByteString
commentEx =
  "; last modified 1 April \
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
  "; blah\n; woot\n  \n; hah"

skipComments :: Parser ()
skipComments =
  skipMany ( do char ';' <|> char '#'
                skipMany (noneOf "\n")
                skipEOL)

sectionEx :: ByteString
sectionEx =
  "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $
    Section h (M.fromList assignments)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) m =
  M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections =
        foldr rollup M.empty sections
  return $ Config mapOfSections

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

earlyParserTesting :: IO ()
earlyParserTesting = do
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

iniParserTesting =
  hspec $ do
    describe "assignment parsing" $
      it "can parse  a simple assignment" $ do
        let m = parseByteString
                parseAssignment
                mempty assignmentEx
            r' = maybeSuccess m
        print m
        r' `shouldBe` Just ("woot","1")
    describe "header parsing" $
      it "can parse a simple header" $ do
        let m =
              parseByteString
              parseHeader
              mempty headerEx
            r' = maybeSuccess m
        print m
        r' `shouldBe` Just (Header "blah")
    describe "comment parsing" $
      it "skips comment before header" $ do
        let p = skipComments >> parseHeader
            i = "; woot\n[blah]"
            m = parseByteString p mempty i
            r' = maybeSuccess m
        print m
        r' `shouldBe` Just (Header "blah")

    describe "Section parsing" $
      it "can parse a simple section" $ do
        let m = parseByteString parseSection
                mempty sectionEx
            r' = maybeSuccess m
            states =
              M.fromList [("Chris", "Texas")]
            expected' =
              Just (Section (Header "states")
                            states)
        print m
        r' `shouldBe` expected'

    describe "INI Parsing" $
      it "can parse multiple sections" $ do
        let m =
              parseByteString parseIni
              mempty sectionEx''
            r' = maybeSuccess m
            sectionValues =
              M.fromList
              [ ("alias", "claw")
              , ("host", "wikipedia.org")]
            whatisitValues =
              M.fromList
              [("red", "intoothandclaw")]
            expected' =
              Just (Config
                    (M.fromList
                     [ (Header "section"
                        , sectionValues)
                     , (Header "whatisit"
                        , whatisitValues)]))
        print m
        r' `shouldBe` expected'

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)


instance Ord NumberOrString where
  compare nos nos' =
    case (nos, nos') of
      -- numeric identifiers always have lower precendence than non-numeric
      (NOSI _, NOSS _) -> LT
      (NOSS _, NOSI _) -> GT
      (NOSI i, NOSI i') -> compare i i'
      (NOSS s, NOSS s') -> compare s s'

eqPass :: Ord a => a -> a -> Ordering -> Ordering
eqPass a b f =
  case compare a b of
    EQ -> f
    x -> x

instance Ord SemVer where
  compare (SemVer major minor patch release _)
          (SemVer major' minor' patch' release' _) =
    let
      cmpRelease = eqPass release release' EQ
      cmpPatch = eqPass patch patch' cmpRelease
      cmpMinor = eqPass minor minor' cmpPatch
    in
      eqPass major major' cmpMinor

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  release <- optional parseRelease
  metadata <- optional parseMetadata
  let fm = fromMaybe []
  return $
    SemVer major minor patch (fm release) (fm metadata)

parseRelease :: Parser Release
parseRelease = do
  char '-'
  sepBy parseNumberOrString (symbol ".")

parseMetadata :: Parser Release
parseMetadata = do
  char '+'
  sepBy parseNumberOrString (symbol ".")

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = do
  identifier <- token (many parseIdentifierChar)
  return $ case (readMaybe identifier) of
    Just i -> NOSI i
    Nothing -> NOSS identifier

parseIdentifierChar :: CharParsing m => m Char
parseIdentifierChar =
  satisfy p <?> "letter, digit, or hyphen"
  where
    p x =
      isAlphaNum x || (x == '-')

psv = maybeSuccess . parseString parseSemVer mempty

chEx1Semver =
  hspec $ do
    it "passes the specified examples" $ do
      psv "2.1.1" `shouldBe` Just (SemVer 2 1 1 [] [])
      psv "1.0.0-x.7.z.92"  `shouldBe`  Just (SemVer 1 0 0
                                               [NOSS "x",
                                                NOSI 7,
                                                NOSS "z",
                                                NOSI 92] [])
      psv "1.0.0-gamma+002" `shouldBe` Just (SemVer 1 0 0
                                             [NOSS "gamma"] [NOSI 2])
      psv "1.0.0-beta+oof.sha.41af286" `shouldBe`
        Just (SemVer 1 0 0
                 [NOSS "beta"]
                 [NOSS "oof",
                  NOSS "sha",
                  NOSS "41af286"])

    it "ordering" $ do
      let big = SemVer 2 1 1 [] []
      let little = SemVer 2 1 0 [] []
      compare big little `shouldBe` GT

    it "also passes my examples" $ do
      psv "2.1.1-alpha" `shouldBe` Just (SemVer 2 1 1 [NOSS "alpha"] [])
      psv "2.1.1-alpha.1" `shouldBe` Just (SemVer 2 1 1 [NOSS "alpha", NOSI 1] [])

      let sv = SemVer 2 1 1 [NOSI 1] []
      let sv' = SemVer 2 1 1 [NOSS "alpha"] []
      compare sv sv' `shouldBe` LT

      let sv = SemVer 2 1 1 [] [NOSS "foo"]
      let sv' = SemVer 2 1 1 [] [NOSS "bar"]
      compare sv sv' `shouldBe` EQ

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"


-- base10Integer :: Parser Integer
-- base10Integer = do
--   i <- many parseDigit
--   let mi = readMaybe i :: Maybe Integer
--   maybe (fail $ "could not parse integer from " ++ i) return mi

base10Integer :: Parser Integer
base10Integer = do
  i <- many parseDigit
  maybe (fail $ "could not parse integer from " ++ i) return $ digitsStringToInteger i

-- like previous, but will parse a negative, too
base10Integer' :: Parser Integer
base10Integer' = do
  neg <- optional (char '-')
  int <- base10Integer
  case neg of
    Just _ -> pure $ int * (-1)
    Nothing -> pure int

-- there are other ways to do this, but the end of ex2 mentions "accumulating"
-- in order to parse, so there is an assumption that I should do it more manually
-- instead of using readMaybe, or whatever (which i did before i rewrote this!)
-- point being if should not use readMaybe for the earlier, i should not
-- use it for this sub problem,  i guess
charToNum :: Char -> Maybe Integer
charToNum x =
  case x of
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    _   -> Nothing

digitsStringToInteger :: String -> Maybe Integer
digitsStringToInteger "" = Nothing
digitsStringToInteger str =
  foldl' folder (Just 0) $ fmap charToNum str
  where folder x y =
          case (x, y) of
            (Just x, Just y) -> Just $ (x * 10) + y
            _ -> Nothing

pd = maybeSuccess . parseString parseDigit mempty
pint = maybeSuccess . parseString base10Integer mempty
pint' = maybeSuccess . parseString base10Integer' mempty


chEx3IntegerNeg = hspec $ do
  it "exercise 3 parses negatives" $ do
    pint' "123abc" `shouldBe` Just 123
    pint' "-123abc" `shouldBe` Just (-123)


chEx2Integer = hspec $ do
  describe "integer parser" $ do
    it "parses examples" $ do
      pd "123" `shouldBe` Just '1'
      pd "abc" `shouldBe` Nothing
      pint "123abc" `shouldBe` Just 123
      pint "abc" `shouldBe` Nothing

chEx3Integer :: IO ()
chEx3Integer = hspec $ do
  let pd = maybeSuccess . parseString parseDigit mempty
  describe "integer parser + negative" $ do
    it "parses" $ do
      putStrLn "hullo"
      1 `shouldBe` 1 :: Expectation

main :: IO ()
main = do
  earlyParserTesting
  iniParserTesting
  chEx1Semver
  chEx2Integer
  chEx3Integer
