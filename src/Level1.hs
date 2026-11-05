-- | Домашка 6. Классы типов: обязательные задачи.
module Level1 where

import Data.Kind (Type)
import Examples qualified as E
import MetaUtils (todo, todoImpl)


-- 1.1. Прочитайте src/Examples.hs до запуска.
-- Запишите результаты литералами и конструкторами, без вычислений из условия.

-- E.render E.Warning
predictedShow :: String
predictedShow = "[WARNING]"

-- E.Labelled "old" 7 == E.Labelled "new" 7
predictedEq :: Bool
predictedEq = True

-- fmap not (Just True)
predictedFmap :: Maybe Bool
predictedFmap = Just False


-- 1.2. Равенство деревьев.
-- Сравните форму дерева и вторую компоненту пары в каждом узле.
-- Первая компонента пары — служебная метка, она не влияет на равенство.
-- Законы Eq — лекция 6; среди тестов есть Reflexivity, Symmetry, Transitivity.

data IntTree = IntLeaf | IntNode IntTree (Int, Int) IntTree
  deriving Show

{- HLINT ignore "Use record patterns" -}
instance Eq IntTree where
  IntLeaf == IntLeaf = True
  IntNode l (_, x) r == IntNode l' (_, x') r' = l == l' && x == x' && r == r'
  _ == _ = False


-- 1.3. Functor для дерева с произвольным числом детей.
-- Примените функцию к значению каждого Node. Форму и порядок детей сохраните.
-- deriving Functor вместо собственного инстанса использовать нельзя.

data Tree a = Leaf | Node a [Tree a]
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node x list) = Node (f x) (map (fmap f) list)


-- 1.4. Сравнение структур.
-- Класс StructEq сравнивает форму контейнеров, игнорируя значения элементов.
-- Реализуйте оператор (==#) для списков и Tree, написав инстансы целиком.
-- Типы элементов двух контейнеров могут различаться.
-- Для списков одинаковая структура означает одинаковую длину.
-- Для Tree нужны одинаковые конструкторы и формы детей в том же порядке.

class StructEq (f :: Type -> Type) where
  (==#) :: f a -> f b -> Bool

instance StructEq [] where
  (==#) [] [] = True
  (==#) a b = (length a) == (length b)


instance StructEq Tree where
  (==#) Leaf Leaf = True
  (==#) (Node _ l) (Node _ l') = (==#) l l' && (all (\(a, b) -> (==#) a b) (zip l l'))
  (==#) _ _ = False
