module Part3.Tasks where

import Util (notImplementedYet)
import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = (f n) : finc f (n+1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
splitToDigits :: [Int] -> [Int]
splitToDigits (0:xs) = 0 : splitToDigits xs
splitToDigits (x:xs) = x `mod` 10 : splitToDigits ((x `div` 10) : xs) ++ splitToDigits xs
splitToDigits [] = []

mostFrequentNumberInList :: [Int] -> Int
mostFrequentNumberInList [] = 0
mostFrequentNumberInList xs = number
  where
    groupedNumbers = group (sort xs)
    number = fst (maximumBy (comparing snd) [(head g, length g) | g <- groupedNumbers])

mostFreq :: [Int] -> Int
mostFreq (x:xs) = mostFrequentNumberInList (splitToDigits (x:xs))
mostFreq [] = 0

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
isMember :: (Eq a) => a -> [a] -> Bool
isMember n [] = False
isMember n (x:xs)
  | n == x = True
  | otherwise = isMember n xs

uniq :: (Eq a) => [a] -> [a]
uniq l = foldr (\x acc -> if (isMember x acc) then acc else x:acc) [] l

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
isMemberFst :: (Eq k) => k -> [(k, a)] -> Bool
isMemberFst first ((res, lst) : t)
  | first == res = True 
  | otherwise = isMemberFst first t
isMemberFst p [] = False

addToMember :: (Eq k) => a -> k -> [(k, [a])] -> [(k, [a])]
addToMember x app ((res, lst) : t)
  | app == res = (res, x:lst) : t
  | otherwise = addToMember x app t
addToMember x app [] = []

grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = foldr (\x acc -> if (isMemberFst (f x) acc) then addToMember x (f x) acc else ((f x), [x]) : acc) [] l

