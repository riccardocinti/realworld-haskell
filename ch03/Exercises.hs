import Data.List

-- 1. 2.
customLength :: [a] -> Int
customLength [] = 0
customLength (x:xs) = 1 + customLength (xs)

-- 3.
algebraicMean :: Fractional a => [a] -> a
algebraicMean list = sum list / fromIntegral (length list)

-- 4.
palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = [x] ++ palindrome(xs) ++ [x]

-- 5.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [a] = True
isPalindrome [a, b] = a == b
isPalindrome list = head list == last list && isPalindrome (tail (init list))

-- 6.
sortByLength :: Foldable t => [t a] -> [t a]
sortByLength list = sortBy (\a b -> compare (length a) (length b)) list

-- 7
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = x
myIntersperse delim (x:xs) = x ++ [delim] ++ myIntersperse delim xs
