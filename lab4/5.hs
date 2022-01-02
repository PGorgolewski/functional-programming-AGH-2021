newtype MyInt = MkMyInt Int 

instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
    negate (MkMyInt i1)           = MkMyInt (negate i1)
    abs (MkMyInt i1)              = MkMyInt (abs i1)
    signum (MkMyInt i1)           = MkMyInt (signum i1)
    fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
    show (MkMyInt i1) = "MkMyInt " ++ show i1
