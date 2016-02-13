myLast :: [a] -> a 
myLast (x:[]) = x
myLast (x:xs) = myLast xs
myLast [] = error "list is empty"

myLast' :: [a] -> a
myLast' xs = case xs of
        x:[] -> x
        x:xs -> myLast' xs
        _ -> error "list is empty"

myButLast :: [a] -> a 
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs
myButLast _ = error "list is too short"

elementat :: [a] -> Int -> a
elementat (x:_) 1 = x
elementat (x:xs) y = elementat xs (y-1)
elementat [] _ = error "list is empty"

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

myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

myReverse' :: [a] -> [a]
myReverse' = foldl (\acc -> \x -> x:acc) []

myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs
