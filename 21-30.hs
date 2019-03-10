module Questoes where

--21 delete

mydelete :: Eq a => a -> [a] -> [a]
mydelete a [] = []
mydelete a (h : t) | a==h = t
                   | otherwise = h : mydelete a t

--22 (\\)

myslashslash :: Eq a => [a] -> [a] -> [a]
myslashslash l [] = l
myslashslash [] l = []
myslashslash l (h : t) = myslashslash (mydelete h l) t

--23 union

myunion :: Eq a => [a] -> [a] -> [a]
myunion l [] = l
myunion l (h:t) | elem h t == True = myunion l t
                | otherwise = myunion (l ++ [h]) t  

--24 intersect

myintersect :: Eq a => [a] -> [a] -> [a] 
myintersect [] [] = []
myintersect [] l = []
myintersect (h : t) l | (aux5 h l == True) = h : myintersect t l
                      | otherwise = myintersect t l

aux5 :: Eq a => a -> [a] -> Bool
aux5 a [] = False
aux5 a (i : j) | a==i = True
               | otherwise = aux5 a j

--25 insert

myinsert :: Ord a => a -> [a] -> [a]
myinsert a [] = [a]
myinsert a (h : t) | a>=h = h : myinsert a t
                   | otherwise = a : (h : t)

--26 unwords

myunwords :: [String] -> String
myunwords [x] = x
myunwords [] = []
myunwords (h : t) = h ++ " " ++ myunwords t

--27 unlines

myunlines :: [String] -> String
myunlines [x] = x  ++ "\n"
myunlines [] = []
myunlines (h : t) = h ++ "\n" ++ myunlines t

--28 pMaior

pMaior :: Ord a => [a] -> Int
pMaior (h : t) = aux7 0 (aux6 h t) (h : t)

aux6 :: Ord a => a -> [a] -> a
aux6 x [] = x
aux6 a (i : j) | a>=i = aux6 a j
               | otherwise = aux6 i j

aux7 :: Ord a => Int -> a -> [a] -> Int
aux7 x a (h : t) | (a==h) = x
                 | otherwise = aux7 (x+1) a t 

--29 temRepetidos

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos [a] = False
temRepetidos (h : t) | (aux8 h t == True) = True
                     | otherwise = temRepetidos t

aux8 :: Eq a => a -> [a] -> Bool
aux8 a [] = False
aux8 a (i : j) | a==i = True
               | otherwise = aux8 a j

--30 algarismos	

myalgarismos :: [Char] -> [Char]
myalgarismos [] = []
myalgarismos (h : t) | h>='0' && h<='9' = h : myalgarismos t
                     | otherwise = myalgarismos t
