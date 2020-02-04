{-# LANGUAGE RankNTypes #-}
module Practicum2A where

{-
Name:           <Name and family name>
VU-net id:      <VU-net id>
Student number: <Student number>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <in case you used sources such as books or webpages
                 please mention them here>
-}

-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals = nat 1
nat h = h : nat (h+1)

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = 0 : 1 : zeroesandones

-- Exercise 3
threefolds :: [Integer]
threefolds  = map (\x -> x * 3)  (0 : naturals)


-- Exercise 4
nothreefolds :: [Integer]
nothreefolds = filter (\x -> mod x 3  /= 0 ) naturals

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds y =  map (\x -> x * y)  (0 : naturals)

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds y = filter (\x -> mod x y /= 0 ) naturals

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds n  l = filter (\x -> mod x n /= 0 ) l

-- Exercise 8
eratosthenes :: [Integer]
eratosthenes = eratosthenes2 (nat 2)
eratosthenes2 (h:ts) = h : (eratosthenes2 (filter (\x -> mod x  h /= 0) ts)) 

-- Exercise 9
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- -----------------------
-- Exercise Church Numerals
-- -----------------------
-- we need polymorphic types for the Church Numerals 
type ChurchNumeral = forall a . (a -> a) -> a -> a

-- Exercise 1
churchnumeral :: (Eq a, Num a) => a -> ChurchNumeral 
churchnumeral n =
  if   n == 0
  then \s z -> z
  else \s z -> churchnumeral (n - 1) s (s z)

backtointeger :: (Num a) => ChurchNumeral -> a
backtointeger cn = cn (+1) 0

--backtointeger (churchnumeral 3)
{- Show usage here -}

-- Exercise 2
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality x y = backtointeger x == backtointeger y

-- Exercise 3 given as example
successor ::  ChurchNumeral -> ChurchNumeral
successor x s z  = s ( x s z ) 
 
-- Exercise 4
successorb :: ChurchNumeral -> ChurchNumeral
successorb x s z = x s (s z)

-- Exercise 5
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  backtointeger ( f ( churchnumeral n ) ) 

-- Exercise 6
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition x y s z  = x s (y s z)

multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication x y s  = x (y s)

exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation x y  = y x 

-- Exercise 7
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2 f m n  = backtointeger ( f ( churchnumeral m ) ( churchnumeral n ) ) 


-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)
  
single :: a -> BinaryTree a
single x = Node (Leaf) x (Leaf)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes (Leaf) = 0
numberofnodes (Node l x r) = numberofnodes l + numberofnodes r + 1

-- Exercise 2
height :: BinaryTree a -> Integer
height (Leaf) = 0
height (Node l x r) = 1 + if height l > height r then height l else height r 

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes (Leaf) = 0
sumnodes (Node l x r) = sumnodes l + sumnodes r + x

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror (Leaf) = (Leaf)
mirror (Node l x r) = Node (mirror r) x (mirror l) 

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten (Leaf) = []
flatten (Node l x r) = (flatten l) ++ [x] ++ (flatten r)

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap f (Leaf) = (Leaf)
treemap f (Node l x r) = (Node (treemap f l) (f x) (treemap f r))

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan k (Leaf) = True
smallerthan k (Node l x r) = (x < k) && (smallerthan k r)

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan k (Leaf) = True
largerthan k (Node l x r) = (x > k) && (largerthan k l) 

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree (Leaf) = True
isbinarysearchtree (Node l x r) = (smallerthan x l) && (largerthan x r) && (isbinarysearchtree l) && (isbinarysearchtree r)

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement k (Leaf) = False
iselement k (Node l x r) = (iselement k l) || (k == x) || (iselement k r)

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert k (Leaf) = (Node Leaf k Leaf)
insert k (Node l x r) = 
	if k == x
		then (Node l x r)
	else if k > x
		then (Node l x (insert k r))
	else (Node (insert k l) x r)

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree (h:[]) = (Node Leaf h Leaf)
createbinarysearchtree (h:t) = createbinarysearchtree2 t (Node Leaf h Leaf)

createbinarysearchtree2 :: (Ord a, Eq a) => [a] -> BinaryTree a -> BinaryTree a
createbinarysearchtree2 (h:[]) (Node l x r) = insert h (Node l x r)
createbinarysearchtree2 (h:t) (Node l x r) = createbinarysearchtree2 t (insert h (Node l x r))


-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove k (Leaf) = Leaf
remove k (Node l x r) =
	if k < x
		then (Node (remove k l) x r)
	else if k > x
		then (Node l x (remove k r))
	else if k == x
		then removeRoot (Node l x r)
	else
		(Node l x r)
		
removeRoot (Leaf) = Leaf
removeRoot (Node l x r) = 
	if (l == Leaf) && (r == Leaf)
		then Leaf
	else if l == Leaf
		then r
	else if r == Leaf
		then l
	else
		(insertListIntoTree (flatten r) l)
		
insertListIntoTree [] (Leaf) = Leaf
insertListIntoTree [] (Node l x r) = (Node l x r)
insertListIntoTree (h:t) (Leaf) = insertListIntoTree t (insert h (Leaf))
insertListIntoTree (h:t) (Node l x r) = insertListIntoTree t (insert h (Node l x r))