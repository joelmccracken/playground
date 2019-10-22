{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE LambdaCase       #-}

module Chapter24 where

import Text.Trifecta
import Text.Parser.Combinators
import Control.Applicative
import Data.Ratio ((%))
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe, isJust)
import Text.RawString.QQ
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Test.Hspec
import Text.Read (readMaybe)
import Data.Char (isAlphaNum, toLower)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Clock
import Safe (tailMay)
import Data.Word
import Data.Either (isLeft, isRight)
import Data.Bits
import Control.Monad (when)
import Data.Monoid

-- import Control.Applicative.Combinators (count')

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
  skipMany whitespace

whitespace :: Parser Char
whitespace = (char ' ' <|> char '\n')

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


eitherSuccess :: Result a -> Either String a
eitherSuccess (Success a) = Right a
eitherSuccess (Failure err) = Left $ show err

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

iniParserTesting = do
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

chEx1Semver = do
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


stringToInteger :: Integer -> (Char -> Maybe Integer) -> String -> Maybe Integer
stringToInteger _ _ "" = Nothing
stringToInteger base fn str =
  toIntegerFromSplitArabicNumeral base <$> (sequence $ fmap fn str)

toIntegerFromSplitArabicNumeral :: Integer -> [Integer] -> Integer
toIntegerFromSplitArabicNumeral base parts =
  foldl' folder 0 parts
  where
    folder x y = (x * base) + y



digitsStringToInteger :: String -> Maybe Integer
digitsStringToInteger = stringToInteger 10 digitToInteger

pd = maybeSuccess . parseString parseDigit mempty
pint = maybeSuccess . parseString base10Integer mempty
pint' = maybeSuccess . parseString base10Integer' mempty


chEx2Integer = do
  describe "integer parser" $ do
    it "parses examples" $ do
      pd "123" `shouldBe` Just '1'
      pd "abc" `shouldBe` Nothing
      pint "123abc" `shouldBe` Just 123
      pint "abc" `shouldBe` Nothing

chEx3IntegerNeg = do
  it "exercise 3 parses negatives" $ do
    pint' "123abc" `shouldBe` Just 123
    pint' "-123abc" `shouldBe` Just (-123)

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber
  = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  let
    mm :: String -> Parser Int
    mm s = (maybe (fail "programmer error, mm called with unverified-as-only-nums string")
            id
            $ ((return . fromIntegral) <$> digitsStringToInteger s))

    parsePhone1 :: Parser PhoneNumber
    parsePhone1 = do
      _ <- optional $ string "1-"
      c1 <- ((count 3 digit) >>= mm)
      char '-'
      c2 <- ((count 3 digit) >>= mm)
      char '-'
      c3 <- ((count 4 digit) >>= mm)
      pure $ PhoneNumber c1 c2 c3

    parsePhone2 :: Parser PhoneNumber
    parsePhone2 = do
      c1 <- ((count 3 digit) >>= mm)
      c2 <- ((count 3 digit) >>= mm)
      c3 <- ((count 4 digit) >>= mm)
      pure $ PhoneNumber c1 c2 c3

    parsePhone3 :: Parser PhoneNumber
    parsePhone3 = do
      char '('
      c1 <- ((count 3 digit) >>= mm)
      char ')'
      char ' '
      c2 <- ((count 3 digit) >>= mm)
      char '-'
      c3 <- ((count 4 digit) >>= mm)
      pure $ PhoneNumber c1 c2 c3

  try parsePhone1 <|> try parsePhone2 <|> parsePhone3

pp :: String -> Maybe PhoneNumber
pp = maybeSuccess . parseString parsePhone mempty

chEx4Phone = do
  it "parses phone nums according to examples" $ do
    let correct = Just (PhoneNumber 123 456 7890)
    pp "123-456-7890" `shouldBe` correct
    pp "1234567890" `shouldBe` correct
    pp "(123) 456-7890" `shouldBe` correct
    pp "1-123-456-7890" `shouldBe` correct

data ActivityLog =
  ActivityLog
    [ ActivityLogDay ]
  deriving (Show, Eq)

data ActivityLogDay =
  ActivityLogDay
   { dayDate :: Text
   , dayEntires :: [ ActivityLogEntry ]
   }
  deriving (Show, Eq)

data ActivityLogEntry =
  ActivityLogEntry
   { entTime :: TimeOfDay
   , entText :: Text
   }
  deriving (Show, Eq)

parseActivityLog :: Parser ActivityLog
parseActivityLog = do
  days <- many parseActivityLogDay
  return $ ActivityLog days

parseActivityLogDay :: Parser ActivityLogDay
parseActivityLogDay = do
  skipWhitespace
  day <- parseActivityLogDayDate
  entries <- parseActivityLogEntries
  nlOrEOF
  return $ ActivityLogDay day entries

skipCommentsAndWhitespace =
  skipMany (try (whitespace >> return ()) <|> try parseComment)

parseActivityLogDayDate :: Parser Text
parseActivityLogDayDate = do
  _ <- string "# "
  y1 <- parseDigit
  y2 <- parseDigit
  y3 <- parseDigit
  y4 <- parseDigit
  _ <- char '-'
  m1 <- parseDigit
  m2 <- parseDigit
  _ <- char '-'
  d1 <- parseDigit
  d2 <- parseDigit
  skipMany (noneOf "\n")
  nlOrEOF
  return $ pack $ (y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:"")

parseActivityLogEntry :: Parser ActivityLogEntry
parseActivityLogEntry = do
  h1 <- parseDigit
  h2 <- parseDigit
  _ <- char ':'
  m1 <- parseDigit
  m2 <- parseDigit
  _ <- space
  let time = (h1:h2:':':m1:m2:[])
  time' <- (parseTimeM True defaultTimeLocale "%R" time :: Parser TimeOfDay)
  txt <- manyTill anyChar actLogEndOfLineInput
  return (ActivityLogEntry time' $ pack txt)

parseComment :: Parser ()
parseComment = do
  string "--"
  many $ noneOf "\n"
  nlOrEOF

actLogEndOfLineInput :: Parser ()
actLogEndOfLineInput =
  (try nlOrEOF) <|>
  (try parseComment )

nlOrEOF :: Parser ()
nlOrEOF =
  try ((char '\n') >> return ()) <|>
  try eof

parseActivityLogEntries :: Parser [ActivityLogEntry]
parseActivityLogEntries = many parseActivityLogEntry

chEx5logFileParserTest = do
  let
    pale :: String -> Maybe ActivityLogEntry
    pale = maybeSuccess . parseString parseActivityLogEntry mempty

    pale' :: String -> Either String ActivityLogEntry
    pale' = eitherSuccess . parseString parseActivityLogEntry mempty

    paleN :: String -> Maybe [ActivityLogEntry]
    paleN = maybeSuccess . parseString (many parseActivityLogEntry) mempty
  describe "parseActivityLogEntry" $ do
    it "parses a single entry (without comments, + newline)" $ do
      pale "08:00 I did a thing\n" `shouldBe`
        (Just $ ActivityLogEntry (TimeOfDay 8 0 0) "I did a thing")
    it "parses a single entry (without comments)" $ do
      pale' "08:00 I did a thing" `shouldBe`
        (Right $ ActivityLogEntry (TimeOfDay 8 0 0) "I did a thing")

    it "parsers a single entry (with comments)" $ do
      pale "08:00 I did a thing -- did good" `shouldBe`
        (Just $ ActivityLogEntry (TimeOfDay 8 0 0) "I did a thing ")
  describe "parseActivityLogEntries" $ do
    it "parses many entries (without comments)" $ do
      paleN "08:01 so\n22:21 face" `shouldBe`
        (Just
         [ ActivityLogEntry (TimeOfDay 8 1 0) "so"
         , ActivityLogEntry (TimeOfDay 22 21 0) "face"
         ])

    it "parses many entries (some with comments)" $ do
      paleN "08:01 first\n08:02 second -- yep number 2\n08:03 third" `shouldBe`
        (Just
         [ ActivityLogEntry (TimeOfDay 8 1 0) "first"
         , ActivityLogEntry (TimeOfDay 8 2 0) "second "
         , ActivityLogEntry (TimeOfDay 8 3 0) "third"
         ])

    it "parses many entries (all with comments)" $ do
      paleN "08:01 so -- ya \n22:21 face -- cheese" `shouldBe`
        (Just
         [ ActivityLogEntry (TimeOfDay 8 1 0) "so "
         , ActivityLogEntry (TimeOfDay 22 21 0) "face "
         ])

  describe "parseActivityLogDayDate" $ do
    let
      paldd' :: String -> Either String Text
      paldd' = eitherSuccess . parseString parseActivityLogDayDate mempty
    it "parses example without comments" $ do
      paldd' "# 2025-02-05" `shouldBe`
        Right "2025-02-05"

    it "parses example with comments" $ do
      paldd' "# 2025-02-05-- ya i derped" `shouldBe`
        Right "2025-02-05"

  describe "parseActivityLogDay" $ do
    let
      pald' :: String -> Either String ActivityLogDay
      pald' = eitherSuccess . parseString parseActivityLogDay mempty

    it "will parse a day with just the date" $ do
      pald' "# 2025-02-05-- total lazy day" `shouldBe`
        (Right $ ActivityLogDay "2025-02-05" [])
    it "will parse a day with some entries" $ do
      let input =
            [r|# 2025-02-05
08:00 awoke
08:02 did a thing
|]
      pald' input `shouldBe`
        (Right (
            ActivityLogDay
              { dayDate = "2025-02-05"
              , dayEntires =
                  [ ActivityLogEntry {
                        entTime = (TimeOfDay 8 0 0)
                      , entText = "awoke"
                      }
                  , ActivityLogEntry
                      { entTime = (TimeOfDay 8 2 0)
                      , entText = "did a thing"
                      }
                  ]
              }
            ))

    it "will parse a day with some entries and comments" $ do
      let input =
            [r|# 2025-02-05-- less lazy day
08:00 awoke-- hard to do
08:02 did a thing-- was so good
|]
      pald' input `shouldBe`
        (Right (
            ActivityLogDay
              { dayDate = "2025-02-05"
              , dayEntires =
                  [ ActivityLogEntry
                      { entTime = (TimeOfDay 8 0 0)
                      , entText = "awoke"
                      }
                  , ActivityLogEntry
                      { entTime = (TimeOfDay 8 2 0)
                      , entText = "did a thing"
                      }
                  ]
              }
            ))

  describe "parseActivityLog" $ do
    let
      pal' :: String -> Either String ActivityLog
      pal' = eitherSuccess . parseString parseActivityLog mempty

    it "should parse" $ do
       let input =
            [r|
# 2025-02-05-- less lazy day
08:00 awoke-- hard to do
08:02 did a thing-- was so good

# 2025-02-05-- less lazy day
09:00 awoke -- slept in a bit

|]
       pal' input `shouldBe`
        (Right (ActivityLog
          [ ActivityLogDay
              { dayDate = "2025-02-05"
              , dayEntires =
                  [ ActivityLogEntry
                      { entTime = (TimeOfDay 8 0 0)
                      , entText = "awoke"
                      }
                  , ActivityLogEntry
                      { entTime = (TimeOfDay 8 2 0)
                      , entText = "did a thing"
                      }
                  ]
              }
          , ActivityLogDay
             { dayDate = "2025-02-05"
             , dayEntires =
                 [ ActivityLogEntry
                     { entTime = (TimeOfDay 9 0 0)
                     , entText = "awoke "
                     }
                 ]
             }
          ]
          ))


newtype IPAddress =
  IPAddress Word32
  deriving (Eq, Show)

ip4 :: Parser IPAddress
ip4 = do
  b1 <- parseDecimal8Bits
  _ <- char '.'
  b2 <- parseDecimal8Bits
  _ <- char '.'
  b3 <- parseDecimal8Bits
  _ <- char '.'
  b4 <- parseDecimal8Bits
  let
    b1' :: Word32
    b1' = fromIntegral b1
    b2' :: Word32
    b2' = fromIntegral b2
    b3' :: Word32
    b3' = fromIntegral b3
    b4' :: Word32
    b4' = fromIntegral b4
    res = (b1' `shift` 24) +
          (b2' `shift` 16) +
          (b3' `shift` 8) +
          b4'
  pure $ IPAddress res

parseDecimal8Bits :: Parser Word8
parseDecimal8Bits = do
  d1 <- pure <$> digit
  d2 <- optional digit
  d3 <- optional digit
  let d' =
        case d2 of
          Just dig -> d1 ++ pure dig
          Nothing -> d1
  let d'' =
        case d3 of
          Just dig -> d' ++ pure dig
          Nothing -> d'
  let mdig = digitsStringToInteger d''
  dig <- maybe (fail $ "Not parsable as 8 bits: " ++ d'') return mdig
  if ((dig > 255) || (dig < 0)) then
    fail $ "not parsable as 8 bits, not in range: " ++ d''
  else
    return $ fromIntegral dig

chEx6IP4ParserTest = do
  describe "parseDecimal8Bits" $ do
    let p8 = eitherSuccess . parseString parseDecimal8Bits mempty

    it "parses" $ do
      p8 "0" `shouldBe` Right 0
      p8 "255" `shouldBe` Right 255
      p8 "100" `shouldSatisfy` isRight

      p8 "-1" `shouldSatisfy` isLeft
      p8 "256" `shouldSatisfy` isLeft
  describe "parse ip4" $ do
    let pip = eitherSuccess . parseString ip4 mempty
    it "parses" $ do
      pip "172.16.254.1"  `shouldBe` (Right $ IPAddress 2886794753)
      pip "204.120.0.15" `shouldBe` (Right $ IPAddress 3430416399)

digitToIntegerMap :: Map Char Integer
digitToIntegerMap =
  let
    chars = ['0' .. '9']
    nums = [0..]
    cn = zip chars nums
  in
    M.fromList cn

digitToInteger :: Char -> Maybe Integer
digitToInteger =
  (flip M.lookup) digitToIntegerMap

hexDigitToIntegerMap :: Map Char Integer
hexDigitToIntegerMap =
  let
    chars = ['0' .. '9']++['a'..'f']
    nums = [0..]
    cn = zip chars nums
  in
    M.fromList cn

hexDigitToInteger :: Char -> Maybe Integer
hexDigitToInteger =
  (flip M.lookup) hexDigitToIntegerMap

hexDigitStringToInteger :: String -> Maybe Integer
hexDigitStringToInteger = stringToInteger 16 (hexDigitToInteger . toLower)

parseHextet :: Parser Integer
parseHextet = do
  hex <- some hexDigit
  when (length hex > 4) $ fail "string of hex chars too long for hextet"
  maybe (fail $ "non-hex character encountered" <> hex) return $ hexDigitStringToInteger hex

parseHextets :: Parser [Integer]
parseHextets = do
  hx <- parseHextet
  oo <- optional $ try (char ':' *> parseHextets)
  return $ maybe [hx] (hx:) oo

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Show)

parseIp6 :: Parser IPAddress6
parseIp6 = do
  hexen1 <- fromMaybe [] <$> optional parseHextets
  ellipsis <- optional $ string "::"
  hexen2 <- fromMaybe [] <$> optional parseHextets

  allParts <- if (isJust ellipsis) then do
    let ptsLen = length hexen1 + length hexen2
    when (ptsLen > 7) $ fail "ipv6 address is too long, unable to parse"
    let inferred = take (8 - ptsLen) $ repeat 0
    return $ hexen1 ++ inferred ++ hexen2
  else do
    when (length hexen1 /= 8) $ fail "ipv6 address is wrong length"
    return hexen1
  let beginning = take 4 allParts
  let ending = drop 4 allParts

  let msw = toIntegerFromSplitArabicNumeral 65536 beginning
  let lsw = toIntegerFromSplitArabicNumeral 65536 ending
  return $ IPAddress6 (fromIntegral msw) (fromIntegral lsw)



  -- allParts <- case hexen2 of
  --   Just hexen2' -> do
  --     let ptsLen = length hexen1 + length hexen2'
  --     when (ptsLen > 7) $ fail "ipv6 address is too long, unable to parse"
  --     let inferred = take (8 - ptsLen) $ repeat 0
  --     return $ hexen1 ++ inferred ++ hexen2'
  --   Nothing -> do
  --     when (length hexen1 /= 8) $ fail "ipv6 address is wrong length"
  --     return hexen1
  -- let beginning = take 4 allParts
  -- let ending = drop 4 allParts

  -- let msw = toIntegerFromSplitArabicNumeral 65536 beginning
  -- let lsw = toIntegerFromSplitArabicNumeral 65536 ending
  -- return $ IPAddress6 (fromIntegral msw) (fromIntegral lsw)


ip6ToInteger :: IPAddress6 -> Integer
ip6ToInteger (IPAddress6 h l) =
  ((2 ^ 64) *
   (toInteger h)
  ) + toInteger l

testParseFn = \fn -> eitherSuccess . parseString fn mempty

chEx7Ip6Parse = do
  describe "ipv6 parsing" $ do
    let phext = eitherSuccess . parseString parseHextet mempty
    let pip6 s = ip6ToInteger <$> (eitherSuccess $ parseString parseIp6 mempty s)
    it "parses a hex chunk" $ do
      hexDigitStringToInteger "ff" `shouldBe` Just 255
      phext "ffff" `shouldBe` Right 65535
      phext "xfff" `shouldSatisfy` isLeft

    it "parses full ip6" $ do
      pip6 "0:0:0:0:0:0:0:fe01" `shouldBe` Right 65025
      pip6 "0:0:0:0:0:0:ac10:fe01" `shouldBe` Right 2886794753
      pip6 "::ac10:fe01" `shouldBe` Right 2886794753

      pip6 "0:0:0:0:0:ffff:ac10:fe01" `shouldBe` Right 281473568538113
      pip6 "0:0:0:0:0:ffff:cc78:f" `shouldBe` Right 281474112159759
      pip6 "FE80:0000:0000:0000:0202:B3FF:FE1E:8329" `shouldBe` Right 338288524927261089654163772891438416681
      pip6 "FE80::0202:B3FF:FE1E:8329" `shouldBe` Right 338288524927261089654163772891438416681
      pip6 "2001:DB8::8:800:200C:417A"  `shouldBe` Right 42540766411282592856906245548098208122
      pip6 "FE80::" `shouldBe` Right 338288524927261089654018896841347694592

main :: IO ()
main = do
  earlyParserTesting
  hspec $ do
    iniParserTesting
    chEx1Semver
    chEx2Integer
    chEx3IntegerNeg
    chEx4Phone
    chEx5logFileParserTest
    chEx6IP4ParserTest
    chEx7Ip6Parse
