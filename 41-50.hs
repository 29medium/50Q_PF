module Questoes where

--41 constroiMset

constroiMset :: Ord a => [a] -> [(a,Int)]
constroiMset [] = []
constroiMset (h : t) = (h,aux11 h t 1) : constroiMset(aux12 h t)

aux11 :: Ord a => a -> [a] -> Int -> Int
aux11 h [] x = x
aux11 h (i : j) x | h==i = aux11 h j (x+1)
                  | otherwise = aux11 h j x

aux12 :: Ord a => a -> [a] -> [a]
aux12 _ [] = []
aux12 y (x : xs) | y==x = aux12 y xs
                 | otherwise = x : aux12 y xs

--42 partitionEithers

partEithers :: [Either a b] -> ([a],[b])
partEithers [] = ([],[])
partEithers l = (lefts l, rights l)

lefts :: [Either a b] -> [a] 
lefts [] = []
lefts (Left a : t) = a : lefts t
lefts (Right b : t) = lefts t

rights :: [Either a b] -> [b]
rights [] = []
rights (Right b : t) = b : rights t
rights (Left a : t) = rights t

--43 catMaybes

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a : t) = a : catMaybes t
catMaybes (Nothing : t) = catMaybes t

--44 posicao

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h : t) | (isNorte h) = posicao (x,y+1) t
                      | (isSul h) = posicao (x,y-1) t
                      | (isEste h) = posicao (x+1,y) t
                      | (isOeste h) = posicao (x-1,y) t

isNorte Norte = True
isNorte _ = False

isSul Sul = True
isSul _ = False

isEste Este = True
isEste _ = False

isOeste Oeste = True
isOeste _ = False


--45 caminho

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (a,b) | y<b = Norte : caminho (x,y+1) (a,b)
                    | y>b = Sul : caminho (x,y-1) (a,b)
                    | x<a = Este : caminho (x+1,y) (a,b)
                    | x>a = Oeste : caminho (x-1,y) (a,b)
                    | (x==a && y==b) = []

--46 vertical

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) | (isNorte h) || (isSul h) = vertical t
               | otherwise = False 

{- 
isNorte Norte = True
isNorte _ = False

isSul Sul = True
isSul _ = False
-}

--47 maisCentral 

data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral (h : t) = aux13 h t

aux13 :: Posicao -> [Posicao] -> Posicao
aux13 h [] = h
aux13 h (i : j) | (aux14 h < aux14 i) = aux13 h j
                | otherwise = aux13 i j

aux14 :: Posicao -> Int
aux14 (Pos a b) = (abs(a)+abs(b))

--48 vizinhos

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos x (h : t) | ((aux17 x h) || (aux18 x h) == True) = h : vizinhos x t
                   | otherwise = vizinhos x t

aux17 :: Posicao -> Posicao -> Bool
aux17 (Pos a b) (Pos x y) | (a==(x-1) && b==y) || (a==(x+1) && b==y) = True
                          | otherwise = False

aux18 :: Posicao -> Posicao -> Bool
aux18 (Pos a b) (Pos x y) | (b==(y-1) && a==x) || (b==(y+1) && a==x) = True
                          | otherwise = False

--49 mesmaOrdenada

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = False
mesmaOrdenada (h : t) = aux15 h t

aux15 :: Posicao -> [Posicao] -> Bool
aux15 _ [] = True 
aux15 (Pos a b) ((Pos x y) : j) | b==y = aux15 (Pos a b) j
                                | otherwise = False

--50 intercecaoOK

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK (Vermelho : t) = interseccaoOK t
interseccaoOK (_ : t) = aux16 t

aux16 :: [Semaforo] -> Bool
aux16 [] = True
aux16 (Vermelho : t) = aux16 t
aux16 (_ : t) = False
