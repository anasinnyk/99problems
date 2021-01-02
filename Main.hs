-- Problem 1
myLast :: [a] -> a
myLast []     = error "Empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast []  = error "Empty list"
myButLast [_] = error "List with one item"
myButLast (x:xs) | length xs == 1 = x
                 | otherwise = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 0 = x
elementAt [] _ = error "Out of the range"
elementAt (x:xs) i | i < 0     = error "Index must be positive"
                   | otherwise = elementAt xs (i-1)

-- Problem 4
myLength :: [a] -> Int
myLength xs = l xs 0
         where
            l [] s = s
            l (x:xs) s = l xs (s+1)

-- Problem 5
myReverse :: [a] -> [a]
myReverse xs = r xs []
          where
            r [] ys  = ys
            r (x:xs) ys = r xs (x:ys)

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (myReverse xs) == xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List []) = []
flatten (List xs) = foldr (++) [] $ map flatten xs

-- Problem 8
compress :: Eq a => [a] -> [a]
compress xs = myReverse $ c xs []
         where
           c [] ys                   = ys
           c [x] ys                  = x:ys
           c (x:y:xs) ys | x == y    = c (y:xs) ys
                         | otherwise = c (y:xs) (x:ys)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = myReverse $ p xs [[x]]
     where
       p [] ys                          = ys
       p (x:xs) ((z:zs):ys) | z == x    = p xs $ (x:z:zs):ys
                            | otherwise = p xs $ [x]:(z:zs):ys

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode (x:xs) = myReverse $ e xs [(1, x)]
       where
         e [] ys                          = ys
         e (x:xs) ((i, y):ys) | x == y    = e xs $ ((i+1), y):ys
                              | otherwise = e xs $ (1, x):(i, y):ys
