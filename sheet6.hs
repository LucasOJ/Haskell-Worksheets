data Heap a = Bare | Pile a (Heap a) (Heap a)
    deriving (Show)

foldHeap :: (a -> b) -> (b -> b -> b) -> Heap a -> b
foldHeap node pile Bare               = error "Foolish Human"
foldHeap node pile (Pile x Bare Bare) = node x
foldHeap node pile (Pile x Bare r)    = pile (node x) (foldHeap node pile r)
foldHeap node pile (Pile x l Bare)    = pile (node x) (foldHeap node pile l)
foldHead node pile (Pile x l r)       = pile (node x) (pile (foldHeap node pile l) (foldHeap node pile r))

heapSize :: Heap a -> Int
heapSize = foldHeap node pile where
    node :: Num b => a -> b
    node _ = 1

    pile :: Num b => b -> b -> b
    pile x y = x + y
