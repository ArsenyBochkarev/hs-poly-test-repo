{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       constructFromElem :: Int -> Int -> Int -> mx
       constructFromLists :: [[Int]] -> mx
       height :: mx -> Int
       width :: mx -> Int
       getElem :: mx -> Int -> Int -> Int -- В общем случае это неверно: должен быть type parameter от mx
       matrixDeterminant :: mx -> Int

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       constructFromElem _ _ x = x
       constructFromLists x = head (head x)
       height _ = 1
       width _ = 1
       getElem m _ _ = m
       matrixDeterminant x = x
instance Matrix [[Int]] where
       constructFromElem w h x = [[if i == j then x else 0 | j <- [0..w-1]] | i <- [0..h-1]] -- Заполняем по диагонали иксами
       constructFromLists lst = lst
       height m = length m  -- Внешний список это высота
       width m = length (head m) -- Внутренний список -- длина
       getElem m i j = (m !! i) !! j
       matrixDeterminant [] = 1
       matrixDeterminant [[x]] = x
       matrixDeterminant m = sum [(-1)^j * (getElem m 0 j) * matrixDeterminant (minor m j) | j <- [0..(width m)-1]] where
              minor m j = Prelude.map (removeCol j) (tail m) where
                     removeCol j xs = Prelude.take j xs ++ Prelude.drop (j + 1) xs
instance Matrix (SparseMatrix Int) where
       constructFromElem w h x = SparseMatrix {
              sparseMatrixWidth = w,
              sparseMatrixHeight = h,
              sparseMatrixElements = if x /= 0 then Data.Map.fromList [((i, i), x) | i <- [0..min (w - 1) (h - 1)]] -- Заполняем по диагонали иксами
                                     else Data.Map.fromList ([]::[((Int, Int), Int)]) -- Нулями заполнять нет смысла:
                                                                                      -- если элемента нет, он и так считается нулём
       }
       constructFromLists lst = SparseMatrix {
              sparseMatrixWidth = length (head lst),
              sparseMatrixHeight = length lst,
              sparseMatrixElements = Data.Map.fromList [((i, j), val) | (i, row) <- zip [0..] lst, (j, val) <- zip [0..] row, val /= 0]
       }
       height = sparseMatrixHeight 
       width = sparseMatrixWidth
       getElem m i j = findWithDefault 0 (i, j) (sparseMatrixElements m) -- 0 если не нашли
       matrixDeterminant m = matrixDeterminant ([[getElem m i j | j <- [0..(width m)-1]] | i <- [0..(height m)-1]]) -- Выходим в подсчёт для списков,
                                                                                                                    -- т.к. так удобнее

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = constructFromElem w w 1
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = constructFromElem w h 0
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b = constructFromLists 
       [[sum [getElem a i k * getElem b k j | k <- [0..(width a)-1]] | j <- [0..(height b)-1]] | i <- [0..(width a)]]
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = matrixDeterminant
