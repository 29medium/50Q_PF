module Questoes where


--31 posImpares

myposImpares :: [a] -> [a]
myposImpares [] = []
myposImpares [x] = []
myposImpares (h:i:t) = i : myposImpares t

--32 posPares

myposPares :: [a] -> [a]
myposPares [] = []
myposPares [a] = [a]
myposPares (h:i:t) = h : myposPares t

--33 isSorted

myisSorted :: Ord a => [a] -> Bool
myisSorted [] = True
myisSorted [x] = True
myisSorted (h:i:t) | h<=i = myisSorted (i:t)
                   | otherwise = False

--34 iSort

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h : t) | (aux10 h t == True) = h : iSort t
              | otherwise = myinsert h (iSort t)

aux10 :: Ord a => a -> [a] -> Bool
aux10 _ [] = True
aux10 x (i : j) | x<=i = aux10 x j
                | otherwise = False

--35 menor 

menor :: String -> String -> Bool
menor "" "" = False
menor "" l = True
menor l "" = False
menor (h:t) (i:j) | h<i = True
                  | h>i = False
                  | h==i = menor t j

--36 elemMset

elemMset :: Eq a => a -> [(a,Int)] -> Bool
elemMset a [] = False
elemMset a ((x,y) : t) | a==x = True
                       | otherwise = elemMset a t

--37 lengthMset

lengthMset :: [(a,Int)] -> Int
lengthMset [] = 0
lengthMset ((x,y) : t) = y + lengthMset t

--38 converteMset

converteMset :: [(a,Int)] -> [a]
converteMset [] = []
converteMset ((x,y) : t) = aux2 y x ++ converteMset t

aux2 :: Int -> a -> [a]
aux2 0 x = []
aux2 y x = [x] ++ aux2 (y - 1) x

--39 insereMset 

insereMset :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMset a [] = [(a,1)]
insereMset a ((x,y) : t) | (a==x) = ((x,y+1) : t)
                         | otherwise = (x,y) : insereMset a t

--40 removeMset

removeMset :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMset a [] = []
removeMset a ((x,y) : t) | (a==x) && (y>1) = ((x,y-1) : t)
                         | (a==x) && (y==1) = t
                         | otherwise = (x,y) : removeMset a t
