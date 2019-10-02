import Prelude hiding (Bush)

data Bush a = Tip | Node (Bush a) a (Bush a)
    deriving (Show)

instance Functor Bush where
    fmap f Tip = Tip
    fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)
