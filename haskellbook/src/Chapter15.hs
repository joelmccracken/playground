module Chapter15 where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Monoid

main :: IO ()
main = do
  hspec $ do
    describe "monoid test prop" $ do
      it "passes monoidAssoc" $ do
        property (monoidAssoc :: S -> S -> S -> Bool)
      it "passes monoidLeftIdentity" $ do
        property (monoidLeftIdentity :: S -> Bool)
      it "passes monoidRightIdentity" $ do
        property (monoidRightIdentity :: S -> Bool)
    describe "monoid Bull" $ do
      it "passes monoidAssoc" $ do
        property (monoidAssoc :: BullMappend)
      it "fails monoidRightIdentity" $ do
        expectFailure $ property (monoidRightIdentity :: Bull -> Bool)
      it "fails monoidLeftIdentity" $ do
        expectFailure $ property (monoidLeftIdentity :: Bull -> Bool)

    describe "monoid First'" $ do
      it "assoc" $ do
        property (monoidAssoc :: FirstMappend)
      it "leftId" $ do
        property (monoidLeftIdentity :: First' String -> Bool)
      it "rightId" $ do
        property (monoidRightIdentity :: First' String -> Bool)

    describe "chapter exercise semigroups" $ do
      it "Trivial" $ do
        property  (semigroupAssoc :: TrivAssoc)

      it "Identity" $ do
        property  (semigroupAssoc :: IdentityAssoc)

    describe "Optional monoid" $ do
      it "1 <> 1 works (Sum)" $ do
        Only (Sum 1) `mappend` Only (Sum 1)
          `shouldBe` Only (Sum 2)

      it "4 <> 2 works (Product)" $ do
        Only (Product 4) `mappend` Only (Product 2)
          `shouldBe` Only (Product 8)

      it "1 <> nothing works" $ do
        Only (Sum 1) `mappend` Nada
          `shouldBe` Only (Sum 1)
        Only [1] `mappend` Nada
          `shouldBe` Only [1]
        Nada <> Only [1]
          `shouldBe` Only [1]

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
      => Monoid (Optional a) where
  mempty = Nada

  mappend Nada x    = x
  mappend x    Nada = x
  mappend (Only a) (Only b) = Only (mappend a b)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
  mconcat [e, "! he said ",
           adv, " as he jumped into his car ",
           noun, " and drove off with his ",
           adj, " wife."]

type S = String

monoidAssoc :: (Eq a, Monoid a)
            => a -> a -> a
            -> Bool
monoidAssoc a b c =
  a <> (b <> c) == (a <> b) <> c



monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool

monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool

monoidRightIdentity a = (a <> mempty) == a


data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]


instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend x (First' Nada) = x
  mappend (First' Nada) x = x
  mappend x y = x


instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return (First' Nada))
              , (5, return (First' $ Only a)) ]

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

class Semigroup a where
  (<<>>) :: a -> a -> a

--- chapter exercises

data Trivial = Trivial deriving (Eq, Show)


instance Semigroup Trivial where
  _ <<>> _ = Trivial


instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m)
                => m -> m -> m -> Bool

semigroupAssoc a b c =
  (a <<>> (b <<>> c)) == ((a <<>> b) <<>> c)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool



instance Semigroup [a] where
  (<<>>) = (<>)

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <<>> (Identity y) = Identity (x <<>> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
