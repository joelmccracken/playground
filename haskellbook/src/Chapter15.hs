{-# LANGUAGE DeriveGeneric #-}
module Chapter15 where

import GHC.Generics
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Monoid
import qualified Data.Semigroup as SG

main :: IO ()
main = do
  hspec $ do
    describe "monoid test prop" $ do
      it "passes monoidAssoc" $ do
        property (monoidAssoc :: String -> String -> String -> Bool)
      it "passes monoidLeftIdentity" $ do
        property (monoidLeftIdentity :: String -> Bool)
      it "passes monoidRightIdentity" $ do
        property (monoidRightIdentity :: String -> Bool)
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
      it "Trivial assoc" $ do
        property (semigroupAssoc :: TrivAssoc)

      it "Trivial left Id" $ do
        property (monoidLeftIdentity :: Trivial -> Bool)

      it "Trivial right Id" $ do
        property (monoidRightIdentity :: Trivial -> Bool)

      it "Identity" $ do
        property  (semigroupAssoc :: IdentityAssoc)

      it "Identity left id" $ do
        property  (monoidLeftIdentity :: Identity String -> Bool)

      it "Identity right id" $ do
        property  (monoidRightIdentity :: Identity String -> Bool)

      it "Two assoc" $ do
        property (semigroupAssoc :: Two String String -> Two String String -> Two String String -> Bool)

      it "Two left id" $ do
        property (monoidLeftIdentity :: Two String String -> Bool)

      it "Two right id" $ do
        property ( monoidRightIdentity :: Two String String -> Bool)

      it "Three" $ do
        property (semigroupAssoc :: Three String String String -> Three String String String -> Three String String String -> Bool)

      it "Four" $ do
        property (semigroupAssoc :: Four String String String String -> Four String String String String -> Four String String String String -> Bool)

      it "BoolConj assoc" $ do
        property (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)

      it "BoolConj left id" $ do
        property (monoidLeftIdentity :: BoolConj -> Bool)

      it "BoolConj right id" $ do
        property (monoidRightIdentity :: BoolConj -> Bool)

      it "BoolConj Behavior spec" $ do
        (BoolConj True) SG.<> (BoolConj True) `shouldBe` (BoolConj True)
        (BoolConj True) SG.<> (BoolConj False) `shouldBe` (BoolConj False)

      it "BoolDisj" $ do
        property (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)

      it "BoolDisj left id" $ do
        property (monoidLeftIdentity :: BoolDisj -> Bool)

      it "BoolDisj right id" $ do
        property (monoidRightIdentity :: BoolDisj -> Bool)

      it "BoolDisj Behavior spec" $ do
        (BoolDisj True) SG.<> (BoolDisj True) `shouldBe` (BoolDisj True)
        (BoolDisj True) SG.<> (BoolDisj False) `shouldBe` (BoolDisj True)

      it "Or"  $ do
        property (semigroupAssoc
                  :: Or String String
                  -> Or String String
                  -> Or String String
                  -> Bool)

      it "Or spec"  $ do
        (Fst 'a') SG.<> (Snd 'b') `shouldBe` (Snd 'b')
        (Fst 'a') SG.<> (Fst 'b') `shouldBe` (Fst 'b' :: Or Char Char)
        (Snd 'a') SG.<> (Fst 'b') `shouldBe` (Snd 'a')
        (Snd 'a') SG.<> (Snd 'b') `shouldBe` (Snd 'a' :: Or Char Char)

      it "Combine spec"  $ do
        let f = Combine $ \n -> Sum (n + 1)
        let g = Combine $ \n -> Sum (n - 1)
        (unCombine (f SG.<> g) $ 0) `shouldBe` Sum { getSum = 0}
        (unCombine (f SG.<> g) $ 1) `shouldBe` Sum { getSum = 2}
        (unCombine (f SG.<> f) $ 1) `shouldBe` Sum { getSum = 4}
        (unCombine (g SG.<> f) $ 1) `shouldBe` Sum { getSum = 2}

        (unCombine (mappend f mempty) $ 1) `shouldBe` Sum { getSum = 2}

      it "Combine assoc" $ do
        property (combineAssoc :: Char
                               -> Combine Char (Sum Int)
                               -> Combine Char (Sum Int)
                               -> Combine Char (Sum Int)
                               -> Bool)

      it "Combine left id" $ do
        let combineLeftId :: Combine String String -> String -> Bool
            combineLeftId comb val =
              (mempty SG.<> comb) `unCombine` val == comb `unCombine` val
        property combineLeftId

      it "Combine right id" $ do
        let combineLeftId :: Combine String String -> String -> Bool
            combineLeftId comb val =
              (comb SG.<> mempty) `unCombine` val == comb `unCombine` val
        property combineLeftId

      it "Comp spec" $ do
        (unComp (Comp (+1) SG.<> Comp (+2)) 5 `shouldBe` 8)
        (unComp (Comp (/2) SG.<> Comp (+2)) 6 `shouldBe` 4)
        (unComp (Comp (+2) SG.<> Comp (/2)) 6 `shouldBe` 5)

      it "Comp assoc" $ do
        property (compAssoc :: Int
                               -> Comp Int
                               -> Comp Int
                               -> Comp Int
                               -> Bool)

      it "Comp left id" $ do
        let compLeftId :: Comp String -> String -> Bool
            compLeftId comp val =
              (mempty SG.<> comp) `unComp` val == comp `unComp` val
        property compLeftId

      it "Comp right id" $ do
        let compLeftId :: Comp String -> String -> Bool
            compLeftId comp val =
              (comp SG.<> mempty) `unComp` val == comp `unComp` val
        property compLeftId

      it "Validation spec" $ do
        let failure :: String -> Validation String Int
            failure = Failure'
            success :: Int -> Validation String Int
            success = Success'
        success 1 SG.<> failure "blah" `shouldBe` Success' 1
        failure "woot" SG.<> failure "blah" `shouldBe` Failure' "wootblah"
        success 1 SG.<> success 2 `shouldBe` Success' 1
        failure "woot" SG.<> success 2 `shouldBe` Success' 2

      it "validation qc" $ do
        property (semigroupAssoc
                  :: Validation String String
                  -> Validation String String
                  -> Validation String String
                  -> Bool)

      it "mem spec" $ do
        let f'      = Mem $ \s -> ("hi", s + (1 :: Int))
            rmzero  = runMem mempty 0
            rmleft  = runMem (f' <> mempty) 0
            rmright = runMem (mempty <> f') 0
        rmleft  `shouldBe` ("hi", 1)
        rmright `shouldBe` ("hi", 1)
        (rmzero :: (String, Int)) `shouldBe` ("", 0)
        rmleft `shouldBe` runMem f' 0
        rmright `shouldBe` runMem f' 0

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

--- chapter exercises

data Trivial = Trivial deriving (Eq, Show)

instance SG.Semigroup Trivial where
  _ <> _ = Trivial


instance Monoid Trivial where
  mempty = Trivial
  mappend = (SG.<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, SG.Semigroup m)
                => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a SG.<> (b SG.<> c)) == ((a SG.<> b) SG.<> c)


type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool


newtype Identity a = Identity a deriving (Eq, Show)

instance SG.Semigroup a => SG.Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x SG.<> y)

instance (SG.Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity (mempty)
  mappend = (SG.<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (SG.Semigroup a, SG.Semigroup b) => SG.Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 SG.<> a2) (b1 SG.<> b2)

instance (SG.Semigroup a, Monoid a, SG.Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (SG.<>)

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (SG.Semigroup a, SG.Semigroup b, SG.Semigroup c) => SG.Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 SG.<> a2) (b1 SG.<> b2) (c1 SG.<> c2)

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (SG.Semigroup a, SG.Semigroup b, SG.Semigroup c, SG.Semigroup d) => SG.Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2)
    = Four (a1 SG.<> a2) (b1 SG.<> b2) (c1 SG.<> c2) (d1 SG.<> d2)

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

instance SG.Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (SG.<>)

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

instance SG.Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (SG.<>)

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

instance SG.Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _ <> x = x

newtype Combine a b =
  Combine { unCombine :: a -> b }
  deriving (Generic)

instance (SG.Semigroup b) => SG.Semigroup (Combine a b) where
  Combine { unCombine = uc1 } <> Combine { unCombine = uc2 }
    = Combine (\x-> (uc1 x) SG.<> (uc2 x))

instance (Monoid b, SG.Semigroup b) => Monoid (Combine a b) where
  mempty = Combine { unCombine = (const mempty) }
  mappend = (SG.<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    x <- arbitrary
    return $ Combine x

instance Show (Combine a b) where
  show _ = "Combine[a->b]"

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = do
    x <- arbitrary
    return $ Sum x

instance (Arbitrary a, CoArbitrary b) => CoArbitrary (Combine a b)

combineAssoc :: (SG.Semigroup b, Eq b)
             => a
             -> Combine a b
             -> Combine a b
             -> Combine a b
             -> Bool
combineAssoc x a b c =
  unCombine (a SG.<> (b SG.<> c)) x == unCombine ((a SG.<> b) SG.<> c) x

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "comp"

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    x <- arbitrary
    return $ Comp x

instance SG.Semigroup (Comp a) where
  Comp uc1 <> Comp uc2 = Comp (uc1 . uc2)

instance SG.Semigroup a => Monoid (Comp a) where
  mempty = Comp id
  mappend = (SG.<>)

compAssoc :: (Eq a)
          => a
          -> Comp a
          -> Comp a
          -> Comp a
          -> Bool
compAssoc x a b c =
  unComp (a SG.<> (b SG.<> c)) x == unComp ((a SG.<> b) SG.<> c) x


data Validation a b
  = Failure' a
  | Success' b
  deriving (Eq, Show)

instance SG.Semigroup a =>
  SG.Semigroup (Validation a b) where
  (Success' s) <> _ = Success' s
  _ <> (Success' n) = Success' n
  (Failure' a) <> (Failure' b) = (Failure' (a SG.<> b))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Failure' a, Success' b]


newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s-> (mempty, s))
  (Mem a) `mappend` (Mem b) = Mem composed
    where
      composed s = (aa <> ba, as)
        where
        (ba, bs) = b s
        (aa, as) = a bs
