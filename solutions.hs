-- problem 1
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs
myLast [] = error "list is empty"

myLast' :: [a] -> a
myLast' xs = case xs of
        x:[] -> x
        x:xs -> myLast' xs
        _ -> error "list is empty"

myLast'' :: [a] -> a
myLast'' = foldl1 (\_ x -> x)

-- problem 2
myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs
myButLast _ = error "list is too short"

-- problem 3
elementat :: [a] -> Int -> a
elementat (x:_) 1 = x
elementat (x:xs) y = elementat xs (y-1)
elementat [] _ = error "list is empty"

-- problem 4
myLength :: [a] -> Int
myLength xs = sum [1 | x <- xs]

myLength' :: [a] -> Int
myLength' xs = f xs 0
        where
                f :: [a] -> Int -> Int
                f [] n = n
                f (x:xs) n = f xs (succ n)

myLength'' :: [a] -> Int
myLength'' = foldl (\acc -> \x -> acc + 1) 0

myLength''' :: [a] -> Int
myLength''' = foldl (\acc x -> acc + 1) 0

-- problem 5
myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

myReverse' :: [a] -> [a]
myReverse' = foldl (\acc -> \x -> x:acc) []

myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []

-- problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
        | p x       = x : filter' p xs
        | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
        let smallerSorted = quicksort (filter' (<=x) xs)
            biggerSorted = quicksort (filter' (>x) xs)
        in smallerSorted ++ [x] ++ biggerSorted

-- problem 7
-- Elem :: a -> NestedList a
-- List :: [NestedList a] -> NestedList a
data NestedList a = Elem a | List [NestedList a]
        deriving (Show)

flatten :: NestedList a -> NestedList a
flatten (Elem x) = Elem x
flatten (List (List xs):ys) = flatten xs ++ flatten ys
flatten (List (Elem x):ys) = List $ x : flatten ys

-- flatten (Elem x) = Elem x
-- flatten (List xs) = List $ foldr (\x acc -> flatten x : acc) [] xs
