module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist [] = REmpty
listToRlist lst = constructReverselist (reverse lst) where
    constructReverselist [] = REmpty
    constructReverselist (x:xs) = constructReverselist xs :< x

-- Реализуйте все представленные ниже классы (см. тесты)

instance (Eq a, Show a) => Show (ReverseList a) where
    show x = "[" ++ show' x ++ "]"
        where
            show' REmpty = ""
            show' (x :< xs) = if x /= REmpty then show' x ++ "," ++ show xs
                else show xs
instance Eq a => Eq (ReverseList a) where
    (/=) (x1 :< xs1) (x2 :< xs2) = x1 /= x2 && xs1 /= xs2
    (/=) (REmpty :< xs) _ = True
    (/=) _ (REmpty :< xs1) = True
    (/=) REmpty REmpty = False
    (/=) REmpty _ = True
    (/=) _ REmpty = True
    (==) lst1 lst2 = not (lst1 /= lst2)
instance Semigroup (ReverseList a) where
    (<>) lst REmpty = lst
    (<>) lst (x :< xs) = (lst <> x) :< xs
instance Monoid (ReverseList a) where
    mempty = REmpty
instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (xs :< x) = (fmap f xs) :< (f x)
instance Applicative ReverseList where
    pure x = REmpty :< x
    (<*>) REmpty _ = REmpty
    (<*>) _ REmpty = REmpty
    (<*>) (f :< fs) as = (f <*> as) <> fmap fs as
instance Monad ReverseList where
    (>>=) REmpty _ = REmpty
    (>>=) (xs :< x) f = (xs >>= f) <> f x
