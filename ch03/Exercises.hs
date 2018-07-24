import Data.List

-- 1.
-- Write a function that computes the number of elements in a list. 
-- To test it, ensure that it gives the same answers as the standard length function.
-- 2.
-- Add a type signature for your function to your source file. To test it, load the source file into ghci again.
customLength :: [a] -> Int
customLength [] = 0
customLength (x:xs) = 1 + customLength (xs)

-- 3.
-- Write a function that computes the mean of a list, i.e., the sum of all elements in the list divided by its length.
algebraicMean :: Fractional a => [a] -> a
algebraicMean list = sum list / fromIntegral (length list)

-- 4.
-- Turn a list into a palindrome; i.e., it should read the same both backward and forward. 
-- For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].
palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = [x] ++ palindrome(xs) ++ [x]

-- 5.
-- Write a function that determines whether its input list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [a] = True
isPalindrome [a, b] = a == b
isPalindrome list = head list == last list && isPalindrome (tail (init list))

-- 6.
-- Create a function that sorts a list of lists based on the length of each sublist. 
-- (You may want to look at the sortBy function from the Data.List module.)
sortByLength :: Foldable t => [t a] -> [t a]
sortByLength list = sortBy (\a b -> compare (length a) (length b)) list

-- 7.
-- Define a function that joins a list of lists together using a separator value.
-- The separator should appear between elements of the list, but it should not follow the last element. 
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = x
myIntersperse delim (x:xs) = x ++ [delim] ++ myIntersperse delim xs
