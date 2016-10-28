module Traversable where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Optional a =
  Nada
  | Yep a
    deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldr f init Nada = init
  foldr f init (Yep a) = f a init 

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = fmap Yep (f a)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = 
    frequency [ (1, fmap Yep arbitrary),
                (1, return Nada)]

instance Eq (a) => EqProp (Optional a) where (=-=) = eq 
-- type TI = Optional


data List a =
  Nil
  | Const a (List a)
    deriving (Show, Eq)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Const a x) = Const (f a) (fmap f x)

instance Foldable List where
  foldr f init Nil = init
  foldr f init (Const a x) = f a (foldr f init x)

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Const a x) = fmap Const (f a) <*> (traverse f x)

data Three a b c =
  Three a b c
  deriving (Show, Eq)
           
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f init (Three a b c) =  (f c init)

instance Traversable (Three a b) where
  traverse f (Three a b c) = pure (Three a b) <*> (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary =
    do a <- arbitrary
       b <- arbitrary
       c <- arbitrary
       return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq 
type TI = Optional

data Three' a b =
  Three' a b b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
  foldr f init (Three' a b c) = f c init

instance Traversable (Three' a) where
  traverse f (Three' a b c) = pure (Three' a b) <*> (f c)

main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)
  
