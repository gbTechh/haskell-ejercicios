exist::Int->[Int]->Bool
exist _ [] = False
exist n (a:as) =
  if (n == a) then True
  else exist n as

sumLista::[Int]->Int 
sumLista [] = 0
sumLista (a:as) = a + sumLista(as)

prodLista::[Int]->Int
prodLista [] = 1
prodLista (a:as) = a * prodLista(as)

listLength::[Int]->Int
listLength [] = 0
listLength (_:as) = 1 + listLength as

invertLista::[Int]->[Int]
invertLista [] = []
invertLista [x] = [x]
invertLista (a:as) = invertLista as ++ [a]
--invertLista (v) = last v : invertLista (init v)

concatLista::[Int]->[Int]->[Int]
concatLista (a) (b) = a ++ b 

filtrarPositivos::[Int]->[Int]
filtrarPositivos [] = []
filtrarPositivos (a:as)
  | a >= 0 = a : filtrarPositivos as
  | otherwise = filtrarPositivos as


contarOcurrencias::Int->[Int]->Int
contarOcurrencias _ [] = 0
contarOcurrencias n (a:as) 
  | n == a = 1 + contarOcurrencias n as
  | otherwise = contarOcurrencias n as


esPar::Int->Bool
esPar x 
  | x `mod` 2 == 0 = True
  | otherwise = False

filtrarParImpar::Int->[Int]->[Int]
filtrarParImpar _ [] = []
filtrarParImpar n (a:as) 
  | n == 2 && esPar a       = a : filtrarParImpar n as
  | n == 2 && not (esPar a) = filtrarParImpar n as
  | n == 1 && not (esPar a) = a : filtrarParImpar n as
  | n == 1 && esPar a       = filtrarParImpar n as
  | otherwise               = a : as

isOrder::[Int]->Bool
isOrder [] = True
isOrder [_] = True
isOrder (a:as) 
  | a > head as = False
  |otherwise = isOrder (as)
--forma mas reducida: isOrder (a:b:as) = a <= b && isOrder (b:as)

unirListasOrdenadas::[Int]->[Int]->[Int]
unirListasOrdenadas [] [] = []
unirListasOrdenadas [] ys = ys        
unirListasOrdenadas xs [] = xs
unirListasOrdenadas (a:as) (b:bs) 
  | a <= b = a : unirListasOrdenadas as (b:bs)
  | otherwise = b : unirListasOrdenadas bs (a:as)

eliminarDuplicados::[Int]->[Int]
eliminarDuplicados [] = []
eliminarDuplicados [x] = [x]
eliminarDuplicados (a:as) 
  | null as = [a]
  | a == head as = eliminarDuplicados (as)
  | otherwise = a : eliminarDuplicados (as) 

{-
eliminarDuplicados :: [Int] -> [Int]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs)
  | x `elem` xs = eliminarDuplicados xs
  | otherwise = x : eliminarDuplicados xs
-}

rotarLista::[Int]->Int->[Int]
rotarLista as 0 = as
rotarLista as n = rotarLista (last as : init as) (n-1)

main :: IO ()
main = print(rotarLista [1,2,3,4] 3)


--main = print(sumLista [1,2,3,4])
--main = print(prodLista [1,2,3,4])
--main = print(listLength [1,2,3,4,5])
--main = print(invertLista [1,2,3,4,5])
--main = print(concatLista [1,2] [3,4])
--main = print(filtrarPositivos [-1, 0, -1, 2, -3])
--main = print(contarOcurrencias 2 [-1, 0, -1, 2, 2,3 ,2,-3])
--main = print(filtrarParImpar 1 [4,1,2,3,4,5])
--main = print(isOrder [2,1])
--main = print(unirListasOrdenadas [1,2,5,6] [2,3,4])
--main = print(eliminarDuplicados [1,2,2,2,2,3,4])
--main = print(rotarLista [1,2,3,4] 3)
