import Prelude hiding (Monoid, mappend, mempty)
class Pretty a where
    pretty :: a -> String
    thingy :: a -> Int

data Reaction = Happy | Sad | Excited | Angry | Indifferent

instance Pretty Reaction where
    pretty Happy = "Meh:)"
    pretty Sad = "Meh :("
    pretty Excited = "Meh!"
    pretty Angry = "Meh! :()"
    pretty Indifferent = "Meh :|"
    thingy Happy = 1
    thingy Sad = 2

data Suit = Heart | Diamond | Club | Spade
    deriving (Show , Eq)

data Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Show , Eq, Ord )

data Card = Joker | Card (Face, Suit)
    deriving (Show , Eq)

zipWithMe :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithMe _ [] _          = []
zipWithMe _ _ []          = []
zipWithMe f (x:xs) (y:ys) = (f (x) (y)):(zipWithMe f xs ys)

instance Num a => Num [a] where
    xs + ys = zipWithMe (+) xs ys
    xs * ys = zipWithMe (*) xs ys
    xs - ys = zipWithMe (-) xs ys
    abs xs = map (abs) xs
    signum xs = map (signum) xs
    fromInteger x = repeat (fromInteger x)

instance Fractional a => Fractional [a] where
    xs / ys = zipWithMe (/) xs ys
    fromRational x = repeat (fromRational x)

nats :: [Double]
nats = [0..]

ns :: [Double]
ns = (nats * 2) + 1

signs :: [Double]
signs = cycle [1,-1]

pis :: [Double]
pis = (fromInteger 4) / (ns * signs)

myPi :: Int -> Double
myPi n = sum (take n pis)

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

instance Monoid [a] where
    mempty = []
    mappend = (++)

newtype Sum = Sum Int
    deriving (Show)

instance Monoid Sum where
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum (x + y)

crush :: Monoid m => [m] -> m
crush = foldr mappend mempty
