module Part1.Tasks where

import Util(notImplementedYet)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x 1 = x
myPow x y = x * myPow x (y - 1)

myPowDouble :: Double -> Integer -> Double
myPowDouble x 0 = 1
myPowDouble x 1 = x
myPowDouble x y = x * myPowDouble x (y - 1)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin = notImplementedYet

-- косинус числа (формула Тейлора)
-- myCosHelper :: Double -> Integer -> Double
-- myCosHelper x n = sum [fromIntegral ((myPow (-1) k) * (myPowDouble x (2*k))) / fromIntegral (factorial (2*k)) | k <- [0..n]]

myCos :: Double -> Double
myCos x = notImplementedYet

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD 0 0 = 0
myGCD a 0 = abs a
myGCD 0 b = abs b
myGCD a b = if b == 0 then abs a else myGCD (abs b) (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect = notImplementedYet

-- является ли данное число простым?
isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper n x
    | x > round (sqrt (fromIntegral n)) = True
    | n `mod` x == 0 = False
    | otherwise = isPrimeHelper n (x+1)
isPrime :: Integer -> Bool
isPrime n = isPrimeHelper n 2

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea = notImplementedYet

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
