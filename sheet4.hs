import Prelude hiding (sum, product, and , or , all , any, length, foldr , foldl , reverse, filter )

sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Integer] -> Integer
product [] = 1
product (x:xs) = x * (product xs)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ k [] = k
foldr f k (x:xs) = f x (foldr f k xs)

sum' :: [Integer] -> Integer
sum' xs = foldr (+) 0 xs

and :: [Bool] -> Bool
and xs = foldr (&&) True xs

or :: [Bool] -> Bool
or xs = foldr (||) False xs

f :: Int -> Bool
f 0 = False
f _ = True

all :: (a -> Bool) -> [a] -> Bool
all p xs = foldr (\x y -> (&&) (p x) y) True xs

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldr (\x y -> (||) (p x) y) False xs

length :: [a] -> Integer
length xs = foldr (\x y -> y + 1) 0 xs

snoc :: [a] -> a -> [a]
snoc xs x = foldr (\a b -> a:b) [x] xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = snoc (reverse xs) x

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldr (\x y -> if p x then x:y else y) [] xs

group :: Eq a => [a] -> [[a]]
group xs = foldr f k xs where
    f :: Eq a => a -> [[a]] -> [[a]]
    f x [] = [[x]]
    f x ((y:ys):yss) | x == y    = (x:y:ys):yss
                     | otherwise = [x]:((y:ys):yss)
    k :: [[a]]
    k = []

transpose :: [[a]] -> [[a]]
transpose xs = foldr f k xs where
    f :: [a] -> [[a]] -> [[a]]
    f [] _ = []
    f (x:xs) (ys:yss) = (x : ys) : (f (xs) (yss))

    k :: [[a]]
    k = foldr (\_ ys -> []:ys) [] xs
