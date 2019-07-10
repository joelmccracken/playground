{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Chapter16 where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr
{-
exercises: be kind

1. *
2. b: (* -> *), T: (* -> *)
3. * -> * -> *
-}

replaceWithP = const 'p'
rwp = replaceWithP

n = Nothing
w = Just "woohoo"
ave = Just "ave"
lms = [ave, n, w]

ha = Just ["Ha", "Ha"]
lmls = [ha, Nothing, Just []]

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
functorCompose f g x =
  fmap (f . g) x  == fmap f (fmap g x)

functorCompose' :: (Functor f, Eq (f c)) => Fun b c -> Fun a b -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x =
  fmap (f . g) x == fmap f (fmap g x)

newtype Identity a = Identity a
  deriving (Show, Eq, Arbitrary)

instance Functor Identity where
  fmap f (Identity a) =  Identity (f a)

data Pair a = Pair a a
  deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

data Two a b = Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Three' a b = Three' a b b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

data Four a b c d = Four a b c d
  deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

data Four' a b = Four' a a a b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    elements [LolNope, Yeppers a]

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

newtype Constant a b
  = Constant { getConstant :: a}
  deriving (Eq, Show)

data Wrap f a
  = Wrap (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
  fmap fn (Wrap fa) = Wrap (fmap fn fa)

type Nat f g = forall a . f a -> f g

-- chapter exercises

data BoolAndSomethingElse a
  = False' a
  | True' a
  deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap fn (True' a) = True' (fn a)
  fmap fn (False' a) = False' (fn a)

instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    elements [False' a, True' a]

newtype Mu f
  = InF {
     outF :: f (Mu f)
     }

data D = D (Array Word Word) Int Int

data Sum' b a
  = First' a
  | Second' b
  deriving (Eq, Show)

instance Functor (Sum' b) where
  fmap f (First' a) = First' (f a)
  fmap f (Second' b) = Second' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First' a, Second' b]

data Company a b c
  = DeepBlue a b
  | Something c
  deriving (Show, Eq)

instance Functor (Company e e') where
  fmap f (Something c) = Something (f c)
  fmap _ (DeepBlue a b) = DeepBlue a b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Company a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    elements [Something c, DeepBlue a b]

data More b a
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (More a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    elements [L a b a', R b a b']

data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a)  where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Finance, Desk a, Bloor b]

data K a b
  = K a
  deriving (Eq, Show)

instance Functor (K a)  where
  fmap _ (K a) = K a

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return $ K a

newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

instance (Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = do
    b <- arbitrary
    return $ Flip (K b)

newtype EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)


instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return $ GoatyConst b

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap fn (LiftItOut fa) = LiftItOut $ fmap fn fa

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = do
    fa <- arbitrary
    return $ LiftItOut fa

data Parappa f g a
  = DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap fn (DaWrappa fa ga) = DaWrappa (fmap fn fa) (fmap fn ga)

instance (Arbitrary (f a),
          Arbitrary (g a))
  => Arbitrary (Parappa f g a) where
  arbitrary = do
    fa <- arbitrary
    ga <- arbitrary
    return $ DaWrappa fa ga

data IgnoreOne f g a b
  = IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap fn (IgnoringSomething (fa) (gb)) = IgnoringSomething fa (fmap fn gb)

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = do
    fa <- arbitrary
    gb <- arbitrary
    return $ IgnoringSomething fa gb

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => (Arbitrary (Notorious g o a t)) where
  arbitrary = do
    go <- arbitrary
    ga <- arbitrary
    gt <- arbitrary
    return $ Notorious go ga gt

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    ls <- arbitrary
    elements [Nil, Cons a ls]

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gl gl' gl'') = MoreGoats (fmap f gl) (fmap f gl') (fmap f gl'')

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = do
    a <- arbitrary
    gl <- arbitrary
    gl' <- arbitrary
    gl'' <- arbitrary
    elements [NoGoat, OneGoat a, MoreGoats gl gl' gl'']

data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str a) = Print str (f a)
  fmap f (Read fn) = Read (f . fn)


newtype F5 a = F5 ((a -> Int) -> Int)

instance Functor F5 where
  fmap f (F5 g) = F5 (\h -> g (h . f))


main :: IO ()
main = hspec $ do
  describe "chapter content" $ do
    it "example of stacking fmaps" $ do
      let tripFmap = (fmap . fmap . fmap) :: (Char -> Char) -> [Maybe String] -> [Maybe String]
      tripFmap rwp lms `shouldBe` [Just "ppp", Nothing, Just "pppppp"]

    {-
    working out types for nested fmaps
    (fmap . fmap)
    :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)

    fmap :: (Functor f) => (a -> b) -> f a -> f b
    fmap' :: (Functor f') => (a' -> b') -> f' a' -> f' b'
    (.) :: (b -> c) -> (a -> b) -> a -> c

    fmap . fmap ::
    (Functor f, Functor f')
    => ((a -> b) (-> f' (f a) -> f' (f b))
    -> ((a -> b) (-> f a -> f b))
    -> (a -> b) -> f' (f a) -> f' (f b)


    f . g = \x-> f (g x)
    \x -> fmap (fmap x)
    -}

    it "another nested fmap example" $ do
      let dblFmap = (fmap . fmap )
            :: ([String] -> Char) -> [Maybe [String]] -> [Maybe Char]
      dblFmap rwp lmls `shouldBe` [ Just 'p', Nothing, Just 'p' ]
      let tripFmap = (fmap . fmap . fmap)
            :: (String -> Char) -> [Maybe [String]] -> [Maybe String]
      tripFmap rwp lmls `shouldBe` [ Just "pp", Nothing, Just "" ]
      let quadFmap = (fmap . fmap . fmap . fmap)
      quadFmap rwp lmls `shouldBe` [ Just ["pp", "pp"], Nothing, Just [] ]

    it "exercises: heaving lifting" $ do
      -- a
      (fmap (+1) $ read "[1]" :: [Int]) `shouldBe` [2]
      -- b
      (fmap . fmap) (++ ("lol" :: String)) (Just ["Hi,", "Hello"])
        `shouldBe` Just ["Hi,lol", "Hellolol"]
      -- c
      ((*2) . (\x -> x - 2) $ 1) `shouldBe` (-2)
      -- d
      (((return '1' ++) . show)
        ((\x -> [x, 1..3]) 0)) `shouldBe` "1[0,1,2,3]"
      -- e
      ioi <- readIO "1" :: IO Integer
      let changed = read $ ("123"++) (show ioi)
      (*3) changed `shouldBe` 3693
    it "quickcheck example identity" $ do
      property (functorIdentity :: [Int] -> Bool)
    it "quickcheck example compose" $ do
      property ((functorCompose (+1) (*2)) :: [Int] -> Bool)
    it "quickcheck example compose w/ coarbitrary " $ do
      property (functorCompose'  :: Fun Int Int -> Fun Int Int -> [Int] -> Bool)
    describe "exercises: instances of func" $ do
      describe "identity" $ do
        it "id" $ do
          property (functorIdentity :: Identity Int -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int Int -> Fun Int Int -> Identity Int -> Bool)
      describe "pair" $ do
        it "id" $ do
          property (functorIdentity :: Pair Int -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int Int -> Fun Int Int -> Pair Int -> Bool)
      describe "Two" $ do
        it "id" $ do
          property (functorIdentity :: Two Int Char -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int Char -> Fun Integer Int -> Two Char Integer -> Bool)
      describe "Three" $ do
        it "id" $ do
          property (functorIdentity :: Three Int Char Char -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int Char -> Fun Integer Int -> Three Char () Integer -> Bool)
      describe "Three'" $ do
        it "id" $ do
          property (functorIdentity :: Three' Int Char -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int Char -> Fun Integer Int -> Three' Char Integer -> Bool)
      describe "Four" $ do
        it "id" $ do
          property (functorIdentity :: Four Int Char Char Integer -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int Char -> Fun Integer Int -> Four Char () String Integer -> Bool)
      describe "Four'" $ do
        it "id" $ do
          property (functorIdentity :: Four' Int Integer -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int Char -> Fun String Int -> Four' Char String -> Bool)

      {- you cannot implement Functor for Trivial as its kind is * -}
    describe "Exercise: Possibly" $ do
      it "id" $ do
        property (functorIdentity :: Possibly Char -> Bool)
      it "compose" $ do
        property (functorCompose' :: Fun Int Char -> Fun String Int -> Possibly String -> Bool)

    describe "Short Exercise" $ do
      describe "1. Functor for Sum" $ do
        it "id" $ do
          property (functorIdentity :: Sum Char Int -> Bool)
        it "compose" $ do
          property (functorCompose' :: Fun Int String -> Fun String Int -> Sum Integer String-> Bool)
      it "2. functor requires kind * -> * and Sum is * -> * -> * so a type needs to be applied to make it fit. This type can only be applied to the first argument." True
    describe "chapter exercises" $ do
      describe "is valid functor possible" $ do
        it "1. Bool can't be made into a functor. It can be used with Const though" True
        describe "2. BoolAndSomethingElse can be made into a functor, tested below" $ do
          it "id" $ do
            property (functorIdentity :: BoolAndSomethingElse Char -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int String
                                      -> Fun String Int
                                      -> BoolAndSomethingElse String
                                      -> Bool)
        it "3. BoolAndMaybeSomethingElse is the same as Possibly from earlier, above, so yes" True
        it "4. Mu f has kind (* -> *) -> *, which does not match * -> *" True
        it "4. D has kind *, which cannot itself be made into a functor" True
      describe "rearrange the arguments to the type constructor so functor instance works" $ do
        describe "1. Sum (Sum' because Sum already used in chapter)" $ do
          it "id" $ do
            property (functorIdentity :: Sum' Char Int -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun String Int -> Sum' Char String -> Bool)
        describe "2. Company" $ do
          it "id" $ do
            property (functorIdentity :: Company Char Int String -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun String Int -> Company Int Char String -> Bool)
        describe "3. More" $ do
          it "id" $ do
            property (functorIdentity :: More Char String -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun String Int -> More Char String -> Bool)
      describe "write functor instances for the following datatypes" $ do
        describe "Quant" $ do
          it "id" $ do
            property (functorIdentity :: Quant Char String -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun String Int -> Quant Char String -> Bool)
        describe "K" $ do
          it "id" $ do
            property (functorIdentity :: K Char String -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun String Int -> K Char String -> Bool)
        describe "Flip K" $ do
          it "id" $ do
            property (functorIdentity :: (Flip K Char String) -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun String Int -> (Flip K Char String) -> Bool)
        describe "EvilGoateeConst" $ do
          it "id" $ do
            property (functorIdentity :: (EvilGoateeConst Char String) -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun String Int -> (EvilGoateeConst Char String) -> Bool)
        describe "LiftItOut" $ do
          it "id" $ do
            property (functorIdentity :: LiftItOut [] Int -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun Int Int -> LiftItOut [] Int -> Bool)
        describe "Parappa" $ do
          it "id" $ do
            property (functorIdentity :: Parappa [] [] String -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun Int Int -> Parappa [] [] Int -> Bool)
        describe "IgnoreOne" $ do
          it "id" $ do
            property (functorIdentity :: IgnoreOne [] [] String String -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun Int Int -> IgnoreOne [] [] Int Int -> Bool)
        describe "Notorious" $ do
          it "id" $ do
            property (functorIdentity :: Notorious [] String Int Bool -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun Int Int -> Notorious [] Int Bool Int -> Bool)
        describe "List" $ do
          it "id" $ do
            property (functorIdentity :: List Int -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun Int Int -> List Int -> Bool)
        describe "GoatLord" $ do
          it "id" $ do
            property (functorIdentity :: GoatLord Int -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun Int Int -> GoatLord Int -> Bool)
        describe "GoatLord" $ do
          it "id" $ do
            property (functorIdentity :: GoatLord Int -> Bool)
          it "compose" $ do
            property (functorCompose' :: Fun Int Char -> Fun Int Int -> GoatLord Int -> Bool)
        it "TalkToMe" $  True
