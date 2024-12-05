module Part2.Tasks where

import Util(notImplementedYet)
import Control.Exception (assert)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant lhs) (IntConstant rhs) = IntConstant (lhs + rhs)
(|+|) lhs rhs = BinaryTerm Plus lhs rhs
infixl 7 |+|

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant lhs) (IntConstant rhs) = IntConstant (lhs - rhs)
(|-|) lhs rhs = BinaryTerm Minus lhs rhs
infixl 7 |-|

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant lhs) (IntConstant rhs) = IntConstant (lhs * rhs)
(|*|) lhs rhs = BinaryTerm Times lhs rhs
infixl 8 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = visit varName replacement expression
   where
      -- Visit BinaryTerm
      visit varToReplace replacement (BinaryTerm op innerLHS innerRHS) =
         BinaryTerm op (replaceVar varToReplace replacement innerLHS) (replaceVar varToReplace replacement innerRHS)

      -- Visit Variable, IntConstant
      visit varToReplace replacement expression =
         if expression == (Variable varToReplace) then replacement
         else expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Plus innerLHS innerRHS) = (evaluate innerLHS) |+| (evaluate innerRHS)
evaluate (BinaryTerm Minus innerLHS innerRHS) = (evaluate innerLHS) |-| (evaluate innerRHS)
evaluate (BinaryTerm Times innerLHS innerRHS) = (evaluate innerLHS) |*| (evaluate innerRHS)
evaluate e = e
