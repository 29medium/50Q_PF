module Questoes where

--11 interspace 

myinterspace :: a -> [a] -> [a]
myinterspace a [] = []
myinterspace a [x] = [x]
myinterspace a (h : t) = h : a : myinterspace a t 

--12 group

mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (h : t) = (aux9 h t) : mygroup(drop(length(aux9 h t))(h : t))

aux9 :: Eq a => a -> [a] -> [a]
aux9 a [] = [a]
aux9 a (i : j) | (a==i) = i : aux9 a j
               | otherwise = aux9 a []

--13 concat

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h : t) = h ++ myconcat t

--14 inits 

inits :: [a] -> [[a]]
inits [x] = [[],[x]]
inits l = inits (myinit l) ++ [l]

myinit :: [a] -> [a]
myinit [] = []
myinit [x] = []
myinit (h : t) = h : myinit t

--15 tails

tails :: [a] -> [[a]]
tails [a] = [[a],[]]
tails l = [l] ++ tails (mytail l) 

mytail :: [a] -> [a]
mytail [] = []
mytail (h : t) = t

--16 isPrefixOf

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h : t) (i : j) | h==i = isPrefixOf t j
                           | otherwise = False

--17 isSufixOf

isSufixOf :: Eq a => [a] -> [a] -> Bool
isSufixOf [] [] = True
isSufixOf [] _ = False
isSufixOf _ [] = False
isSufixOf l1 l2 = isPrefixOf (reverse l1) (reverse l2)

--18 isSubsequenceOf

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [x] [] = False
isSubsequenceOf [] [x] = True
isSubsequenceOf (h : t) (i : j) | h==i = isSubsequenceOf t j
                                | otherwise = isSubsequenceOf (h : t) j

--19 elemIndices

myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices _ [] = []
myelemIndices a l = aux3 0 a l

aux3 :: Eq a => Int -> a -> [a] -> [Int]
aux3 _ _ [] = []
aux3 x a (h : t) | (a == h) = x : aux3 (x+1) a t
                 | otherwise = aux3 (x+1) a t

--20 nub

mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (h : t) = h : mynub (aux4 h t)

aux4 :: Eq a => a -> [a] -> [a]
aux4 _ [] = []
aux4 a (i : j) | (a==i) = aux4 a j
               | otherwise = i : aux4 a j
