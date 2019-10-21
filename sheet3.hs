import Test.QuickCheck

fromJusts :: [Maybe a] -> [a]
fromJusts [] = []
fromJusts (Nothing:xs) = fromJusts xs
fromJusts ((Just x):xs) = x : fromJusts xs

insert :: Int -> [Int] -> [Int]
insert k [] = [k]
insert k (x:xs)| k > x = x : insert k xs
               | otherwise = k : x : xs

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

split :: [a] -> ([a],[a])
split [] = ([],[])
split (x:xs) = (x:ys, zs)
    where
        (zs, ys) = split xs

myRepeat :: a -> [a]
myRepeat x = x :(repeat x)

prop_repeat :: a -> Int -> Property
prop_repeat x n = n >= 0 ==> length (take n (repeat x)) == n

squares :: [Integer]
squares = [x * x | x <- [1..]]

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [(f x)| x <- xs]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = [ x | x<-xs, f x]

cartesian :: [a] -> [b] -> [(a,b)]
cartesian xs ys = [(x,y)|x<-xs, y<-ys]

bitString :: Int -> [[Char]]
bitString 0 = []
bitString 1 = ["0","1"]
bitString n = map ("0"++) (bitString (n-1)) ++ map ("1"++) (bitString (n-1))

bitStrings :: Int -> [[Char]]
bitStrings 0 = []
bitStrings n = (bitStrings (n - 1)) ++ (bitString n)

applies1 :: [(a -> b)] -> [a] -> [b]
applies1 fs xs = [ f x | f <- fs, x <- xs ]

applies2 :: [(a -> b)] -> [a] -> [b]
applies2 [] _ = []
applies2 (f:fs) xs = (map f xs) ++ (applies2 fs xs)

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x:y:(interleave xs ys)
