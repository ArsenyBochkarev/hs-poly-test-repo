module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f (f init x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f init [] = init
myFoldr f init (x:xs) = f x (myFoldr f init xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f lst = myFoldl (\acc -> \x -> acc ++ [f x]) [] lst

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f [] = []
myConcatMap f lst = myFoldl (\acc -> \x -> acc <> f x) [] lst

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat lst = myFoldl (\acc -> \elem -> acc <> elem) [] lst

myReverse :: [a] -> [a]
myReverse [] = []
myReverse lst = myFoldl (\acc -> \elem -> [elem] ++ acc) [] lst

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p lst = myFoldl (\acc -> \elem -> if (p elem) then acc ++ [elem] else acc) [] lst

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p lst = myFoldl (\(acc1, acc2) -> \elem -> if (p elem) then (acc1 ++ [elem], acc2) else (acc1, acc2 ++ [elem])) ([], []) lst
