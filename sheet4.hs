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
sum' = foldr (+) 0

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

all :: (a -> Bool) -> [a] -> Bool
all p = foldr ((&&) . p) True

any :: (a -> Bool) -> [a] -> Bool
any p = foldr ((||) . p) False

length :: [a] -> Integer
length = foldr (const (+1)) 0

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

reverse :: [a] -> [a]
reverse = foldr (flip snoc) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr consCheck [] where
    --consCheck :: a -> [a] -> [a]
    consCheck y ys | p y       = y:ys
                   | otherwise = ys

group :: Eq a => [a] -> [[a]]
group = foldr f [] where
    f :: Eq a => a -> [[a]] -> [[a]]
    f x [] = [[x]]
    f x ((y:ys):yss) | x == y    = (x:y:ys):yss
                     | otherwise = [x]:((y:ys):yss)

transpose :: [[a]] -> [[a]]
transpose = foldr f [] where
    f :: [a] -> [[a]] -> [[a]]
    f xs []  = map (:[]) xs
    f xs xss = zipWith (:) xs xss

