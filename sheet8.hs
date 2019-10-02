type ListSet a = [a]

insert :: Eq a => a -> ListSet a -> ListSet a
insert x xs | (length (filter (\y -> x == y) xs) == 0) = x:xs
            | otherwise = xs

union :: Eq a => ListSet a -> ListSet a -> ListSet a
union xs [] = xs
union [] ys = ys
union (x:xs) ys = union xs (insert x ys)