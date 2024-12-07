module Part1.Tasks where

import Util(notImplementedYet)
import Data.Fixed(mod')

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
mySinHelper :: Double -> Integer -> Double
mySinHelper x n = sum [(myPowDouble (-1) k) * (myPowDouble x (2 * k + 1)) / fromIntegral (factorial (2 * k + 1)) | k <- [0..n]]

mySin :: Double -> Double
mySin x = mySinHelper (mod' x (2 * pi)) 40
-- mySin = notImplementedYet

-- косинус числа (формула Тейлора)
myCosHelper :: Double -> Integer -> Double
myCosHelper x n = sum [(myPowDouble (-1) k) * ((myPowDouble x (2 * k)) / fromIntegral (factorial (2 * k))) | k <- [0..n]]

myCos :: Double -> Double
myCos x = myCosHelper (mod' x (2 * pi)) 20
-- myCos = notImplementedYet

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD 0 0 = 0
myGCD a 0 = abs a
myGCD 0 b = abs b
myGCD a b = if b == 0 then abs a else myGCD (abs b) (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
daysInMonth :: Integer -> Integer -> Integer
daysInMonth year month
    | month == 2 = if isLeapYear year then 29 else 28
    | month `elem` [4, 6, 9, 11] = 30
    | otherwise = 31
    where
        isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = year >= 0 && month >= 1 && month <= 12 && day >= 1 && day <= (daysInMonth year month)

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
shapeArea points = 0.5 * abs (sum [(x1 * y2) - (x2 * y1) | ((x1, y1), (x2, y2)) <- zip points (tail points ++ [head points])])

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
isTriangle :: Double -> Double -> Double -> Bool
isTriangle x y z = x + y > z && x + z > y && y + z > x

triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | a <= 0 || b <= 0 || c <= 0 = -1  -- Если одна из сторон неположительна
    | not (isTriangle a b c) = -1       -- Если стороны не могут образовать треугольник
    -----------------------------------------
    -- Прямоугольный? Теорема Пифагора
    | a * a + b * b == c * c = 2
    | a * a + c * c == b * b = 2
    | a * a  == c * c + b * b = 2
    -----------------------------------------
    -- Тупоугольный? Сумма квадратов двух меньших стороны меньше квадрата большей стороны
    -- a -- наибольшая сторона
    | a > b && a > c && a * a > b * b + c * c = 0
    -- b -- наибольшая сторона
    | b > a && b > c && b * b > a * a + c * c = 0
    -- c -- наибольшая сторона
    | c > a && c > b && c * c > a * a + b * b = 0
    -----------------------------------------
    | otherwise = 1 -- Остроугольный
