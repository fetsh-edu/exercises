{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)
    , dragonFight

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding
    ) where
import Data.Foldable (Foldable(foldl'))
import Data.Char (isSpace)
import Data.List (sort, minimumBy, maximumBy)
import Data.Function (on)


{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct = go 1
    where
      go result [] = result
      go _ (0:_) = 0
      go result (x:xs) = go (result * x) xs

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate = concatMap (replicate 2)

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}

-- Bad
removeAt' :: (Eq a, Num a) => a -> [b] -> (Maybe b, [b])
removeAt' ind list = (rElem, reverse rList)
    where (_, rElem, rList) = foldl' foldF (-1, Nothing, []) list
          foldF (c, mEl, rL) el = if (c + 1) == ind
                             then (c + 1, Just el, rL)
                             else (c + 1, mEl, el:rL)

-- Still bad, as we are traversing the list three times I belive
removeAt'' :: Int -> [b] -> (Maybe b, [b])
removeAt'' ind list = (list !? ind, take ind list ++ drop (ind + 1) list)

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

-- Ok, let's try recursion. Well it's the same as first foldl'
-- but with intermidiate list appending instead of reversing result
-- so Still bad
removeAt''' :: Int -> [b] -> (Maybe b, [b])
removeAt''' = go 0 Nothing []
  where go _ resEl resLi _ [] = (resEl, resLi)
        go c _ resLi ind (x:xs)
          | c == ind = (Just x, resLi ++ xs)
          | otherwise = go (c + 1) Nothing (resLi ++ [x]) ind xs


removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt ind list
    | ind < 0 = (Nothing, list)
    | otherwise = case splitAt ind list of
        (first, []) -> (Nothing, first)
        (first, x:xs) -> (Just x, first ++ xs)


{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}
dropSpaces :: [Char] -> [Char]
dropSpaces = takeWhile (not . isSpace) . dropWhile isSpace

{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
  * As a reward, knight takes all the gold, the treasure and experience.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons has only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay dragon with their sword. Each sword strike
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strike decreases "knight's endurance" by one.
    If knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you find the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

-- some help in the beginning ;)
data Knight = Knight
    { knightHealth    :: Int
    , knightAttack    :: Int
    , knightEndurance :: Int
    }

data Color = Red | Black | Green

data Dragon = Dragon
    { dragonColor  :: Color
    , dragonHealth :: Int
    , dragonAttack :: Int
    }

data Reword = Reward
    { expirience   :: Int
    , gold         :: Bool
    , treasure     :: Bool
    } deriving (Show)

data FightResult
    = KnightWins Reword
    | DragonWins
    | KnightRetreats
    deriving (Show)

dragonReward :: Dragon -> Reword
dragonReward (Dragon Red _ _) = Reward 100 True True
dragonReward (Dragon Black _ _) = Reward 150 True True
dragonReward (Dragon Green _ _) = Reward 250 True False

dragonFight :: Knight -> Dragon -> FightResult
dragonFight knight dragon =
    let toKillaDragon = fromIntegral (dragonHealth dragon) / fromIntegral (knightAttack knight)
        toKillaKnight = fromIntegral (knightHealth knight) / fromIntegral (dragonAttack dragon)
        knightWins = (toKillaDragon, KnightWins (dragonReward dragon))
        dragonWins = (10 * toKillaKnight, DragonWins)
        wagonWills = (fromIntegral (knightEndurance knight) :: Double, KnightRetreats)
        result = snd $ minimumBy (compare `on` fst) [knightWins, dragonWins, wagonWills]
    in result

----------------------------------------------------------------------------
-- Challenges
----------------------------------------------------------------------------

{- The following exercises are considered more challenging. However,
you still may find some of them easier than some of the previous
ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing [x,y] = y > x
isIncreasing (x:rest@(y:_))
    | y < x = False
    | otherwise = isIncreasing rest

{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: [Int] -> [Int] -> [Int]
merge a [] = a
merge [] b = b
merge f@(a:as) s@(b:bs)
    | a < b = a : merge as s
    | otherwise = b : merge f bs

{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where (left, right) = splitAt (length xs `div` 2) xs


{- | Haskell is famous for being a superb language for implementing
compilers and interpeters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
data EvalError
    = VariableNotFound String
    deriving (Show, Eq)

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.
-}
eval :: Variables -> Expr -> Either EvalError Int
eval _ (Lit i) = Right i
eval vars (Var s) = maybeToEither (VariableNotFound s) (lookup s vars)
eval vars (Add e1 e2) = (+) <$> eval vars e1 <*> eval vars e2


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.
-}

-- First take. It works but isn't pretty
constantFolding' :: Expr -> Expr
constantFolding' e = buildExpr' lit vars
    where (lit, vars) = case extractVars' e of
            (0, []) -> (Lit 0, [])
            (0, x:xs) -> (x, xs)
            (i, xs) -> (Lit i, xs)

extractVars' :: Expr -> (Int, [Expr])
extractVars' (Lit i) = (i, [])
extractVars' var@(Var _) = (0, [var])
extractVars' (Add e1 e2) = (fst res1 + fst res2, snd res1 ++ snd res2)
  where res1 = extractVars' e1
        res2 = extractVars' e2

buildExpr' :: Expr -> [Expr] -> Expr
buildExpr' e1 [] = e1
buildExpr' e1 (e2:es) = Add e1 (buildExpr' e2 es)


-- Second take. Still works, is prettier (a bit), but feels wrong anyway.
constantFolding :: Expr -> Expr
constantFolding e = lit `add` vars
    where (lit, vars) = extractVars e

extractVars :: Expr -> (Expr, Expr)
extractVars (Lit i) = (Lit i, Lit 0)
extractVars var@(Var _) = (Lit 0, var)
extractVars (Add e1 e2) = (fst res1 `add` fst res2, snd res1 `add` snd res2)
  where res1 = extractVars e1
        res2 = extractVars e2

add :: Expr -> Expr -> Expr
Lit 0 `add` a = a
b `add` Lit 0 = b
Lit a `add` Lit b = Lit (a + b)
x `add` b = Add x b