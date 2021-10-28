{- 

Programação Funcional - Processamento de Listas
Felipe Daniel Dias dos Santos - 11711ECP004
Graduação em Engenharia de Computação - Faculdade de Engenharia Elétrica - Universidade Federal de Uberlândia

-}

--Exercício 2

--Função que retorna a quantidade de elementos maiores que n em uma lista
sup :: Int -> [Int] -> Int
sup n xs = length [x | x <- xs, x > n]

--Exercício 3

--Função que retorna uma lista formada pelos elementos maiores que n em uma lista
listaSup :: Int -> [Int] -> [Int]
listaSup n xs = [x | x <- xs, x > n]

--Exercício 4

--Função que recebe duas listas e realiza a multiplicação de um elemento da primeiro por todos os elementos da segunda, 
--retornando uma lista de listas
mult :: [Int] -> [Int] -> [[Int]]
mult xs ys = [[x * y | y <- ys] | x <- xs]

--Exercício 5

--Função que verifica se todos os elementos de uma lista são iguais
compara :: Eq a => [a] -> [Char]
compara [] = "Lista vazia."
compara [_] = "Elementos iguais."
compara (x:xs) | x /= head xs = "Elementos distintos."
compara (x:xs) | otherwise = compara xs

--Exercício 6

--Função que verifica se um elemento pertence a uma lista
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence a (x:xs) | a == x = True
                  | otherwise = pertence a xs

--Função que realiza a operação de união entre dois conjuntos
uniao :: Eq a => [a] -> [a] -> [a]
uniao xs ys = xs ++ [y | y <- ys, not (pertence y xs)]

--Função que realiza a operação de interseção entre dois conjuntos
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao xs ys = [x | x <- xs, pertence x ys]

--Exercício 7

--Função que retorna uma lista formada somente pelos elementos negativos de uma lista
negativos :: [Float] -> [Float]
negativos xs = [x | x <- xs, x < 0]

--Exercício 8

--Função que calcula a distância de uma lista de pontos, dados por tuplas, da origem do plano cartesiano
distancias :: [(Float,Float)] -> [Float]
distancias [] = []
distancias ((x, y) : xys) = sqrt (x ^ 2 + y ^ 2) : distancias xys

--Função semelhante à anterior, porém utilizando compreensão de listas
distancias2 :: [(Float, Float)] -> [Float]
distancias2 xs = [sqrt (x ^ 2 + y ^ 2) | (x, y) <- xs]

--Exercício 9

--Função que produz a tabuada de um número n
tabuada :: Int -> [(Int, Int, Int)]
tabuada n = [(n, x, n * x) | x <- [1..10]]
