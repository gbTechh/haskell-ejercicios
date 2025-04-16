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

intercalar::[Int]->[Int]->[Int]
intercalar [] [] = []
intercalar as [] = as
intercalar [] bs = bs
intercalar (a:as) (b:bs) = a : b : intercalar as bs

maximo::[Int]->Int
maximo [a] = a
maximo (a:as) 
  | a > m = a
  | otherwise = m
  where m = maximo as

pertenece::Int->[Int]->Bool
pertenece n [] = False
pertenece n [x] = n == x
pertenece n (a:as)
  | n == a = True
  | otherwise = pertenece n as

sublista::[Int]->[Int]->Bool
sublista [] _ = True
sublista _ [] = False
sublista as bs
  | length as > length bs = False
  | take (length as) bs == as = True
  | otherwise = sublista as (tail bs)

invertirLista::[Int]->[Int]
invertirLista [] = []
invertirLista as = (last as) : invertirLista (init as)


esPalindromo::[Int]->Bool
esPalindromo as = invertirLista(as) == as
 

longitudCamino :: [(Int, Int)] -> Double
longitudCamino [] = 0.0                  -- Caso base: lista vacía, longitud 0
longitudCamino [_] = 0.0                 -- Caso base: un solo punto, longitud 0
longitudCamino ((x1, y1):(x2, y2):xs) = 
  distancia (x1, y1) (x2, y2) + longitudCamino ((x2, y2):xs)

-- Función auxiliar para calcular la distancia euclidiana
distancia :: (Int, Int) -> (Int, Int) -> Double
distancia (x1, y1) (x2, y2) = 
  sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)

{-
permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]                    -- Caso base: lista vacía
permutaciones xs = [ x:ys | x <- xs, ys <- permutaciones (eliminar x xs) ]


-- Función auxiliar para eliminar la primera ocurrencia de un elemento
eliminar :: Eq a => a -> [a] -> [a]
eliminar _ [] = []
eliminar x (y:ys)
  | x == y = ys
  | otherwise = y : eliminar x ys

par :: Int -> Bool
par x = x `mod` 2 == 0

-}

--EJERCICIOS CON MAX FILTER Y FOLDR FOLDL
maximo2::[Int]->Int
maximo2 [] = error "Lista vacía"
maximo2 (as) = foldl max (head as) as

positivos::[Int]->[Int]
positivos as = filter (> 0) as

sumarLista::[Int]->Int
sumarLista as = foldl (+) 0 as

contarSiImpar :: Int -> Int -> Int
contarSiImpar acc x = if odd x then acc + 1 else acc

contarImpares::[Int]->Int
--contarImpares as = foldl (\acc x -> if x `mod` 2 /= 0 then acc + 1 else acc ) 0 as
contarImpares xs = foldl contarSiImpar 0 xs


main :: IO ()
main = do
  print(contarImpares [1,2,3,4,5])
  --print(sumarLista[1,2,3,4,0,1])
  --print (positivos [-1,0, 1,-3,4])
  --print(maximo2 [1,2,33,4,55]) 

  --print (filter par [1, 2, 3, 4, 5, 6])  -- Salida: [2, 4, 6]





--main = print (filter (\x -> x > 3) [1, 2, 3, 4, 5])  -- Salida: [4, 5]
--pertenece :: Int -> [Int] -> Bool
--pertenece n xs = not (null (filter (== n) xs))

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
--main = print(intercalar [1,2,3,4] [5,8,6,7])
--main = print(maximo [11,2,3,666,444] )
--main = print(pertenece 7 [1,2,5,7,23] )
--main = print(sublista [2,5] [1,2,5,7,23] )
--main = print(esPalindromo [1,2,2,1] )
--main = print (longitudCamino [(1, 2), (4, 6), (7, 10)])
--main = print (permutaciones [1, 2])

{-
contarMayores :: Int -> Int -> Int
contarMayores acc x = if x > 3 then acc + 1 else acc

main :: IO ()
main = print (foldl contarMayores 0 [1, 4, 2, 5, 3])  -- Salida: 2
-}

--main :: IO ()
--main = print (foldl (+) 0 [1, 2, 3, 4])  -- Salida: 10