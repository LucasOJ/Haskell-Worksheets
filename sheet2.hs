fromMaybe :: Int -> Maybe Int -> Int
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y

square :: Int -> Int
square x = x * x

perform :: (Int -> Int) -> Maybe Int -> Maybe Int
perform _ Nothing = Nothing
perform f (Just x) = Just (f x)

data Suit = Hearts | Diamonds | Clubs | Spades
    deriving (Show)
type Pip = Int
type Rank = Either Pip Court
data Card = Joker | Card Suit Rank
    deriving (Show)
type Deck = [Card]
data Court = Ace | Jack | Queen | King
            deriving (Eq, Show)

snap :: Card -> Card -> String
snap Joker Joker = "SNAP"
snap (Card _ (Left l1)) (Card _ (Left l2)) | l1 == l2 = "SNAP"
                                           | otherwise = "..."
snap (Card _ (Right r1)) (Card _ (Right r2)) | r1 == r2 = "SNAP"
                                             | otherwise = "..."
snap _ _ = "..."

removeJokers :: Deck -> Deck
removeJokers [] = []
removeJokers (Joker:xs) = removeJokers xs
removeJokers (x:xs) = x : removeJokers xs

data List a = Empty | Cons a (List a)
    deriving (Show)

toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x Empty) = [x]
fromList (Cons x xs) = x : fromList xs
