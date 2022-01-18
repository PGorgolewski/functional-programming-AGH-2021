newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)


newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Applicative MyTriple where
    pure x = MyTriple (x,x,x)
    (MyTriple a) <*> w = fmap a w

instance Functor MyTriple where
  fmap f (MyTriple a) = MyTriple (f a)