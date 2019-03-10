module Questoes where

--1 enumFromTo

myenumFromTo :: Int -> Int -> [Int]
myenumFromTo a b | (a>b) = []
                 | otherwise = a : myenumFromTo (a+1) b

--2 enumFromThemTo

myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo a b c | a<b && a>c = []
                       | a>b && a<c = []
                       | a==c = [a]
                       | otherwise = a : myenumFromThenTo b (b + (b-a)) c

--3 (++)

myplusplus :: [a] -> [a] -> [a]
myplusplus [] l = l
myplusplus l [] = l
myplusplus (h : t) l = h : myplusplus t l

--4 (!!)

myindex :: [a] -> Int -> a
myindex (h : t) 0 = h
myindex (h : t) a = myindex t (a-1)

--5 reverse

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h : t) = myreverse t ++ [h]

--6 take

mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake x [] = []
mytake a (h : t) = h : mytake (a-1) t

--7 drop

mydrop :: Int -> [a] -> [a]
mydrop 0 l = l
mydrop x [] = []
mydrop a (h : t) = mydrop (a-1) t

--8 zip

myzip :: [a] -> [a] -> [(a,a)]
myzip [] [] = []
myzip l [] = []
myzip [] l = []
myzip (h : t) (i : j) = (h,i) : myzip t j

--9 elem

myelem :: Eq a => a -> [a] -> Bool
myelem x [] = False
myelem a (h : t) | a==h = True
                 | otherwise = myelem a t

--10 replicate

myreplicate :: Int -> a -> [a]
myreplicate 0 a = []
myreplicate x a = a : myreplicate (x-1) a
