import Distribution.Simple.Utils (xargs)
-- product type example (one construktor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja prefix Mk dla k konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- sum type example (two construktors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL = error "head': the empty list has no head"
head' (Cons x xs) = x

data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String 

leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Juliette Bionche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red = "Irene Jacob"

data Cart3DVec' a = MkCart3DVec' a a a

xCoord3D :: Cart3DVec' a -> a
xCoord3D (MkCart3DVec' x _ _) = x

yCoord3D :: Cart3DVec' a -> a
yCoord3D (MkCart3DVec' _ y _) = y

zCoord3D :: Cart3DVec' a -> a
zCoord3D (MkCart3DVec' _ _ z) = z

data Cart3DVec a = MkCart3DVec {x1::a, y1::a, z1::a}

--tutaj trzasnij jabadabadu circle and rectangle

data TrafficLights = Green | Yellow | Reed
actionFor :: TrafficLights -> String
actionFor Green = "GoGo"
actionFor Yellow = "WatchOut"
actionFor Reed = "Stop"