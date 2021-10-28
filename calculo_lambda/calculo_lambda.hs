{- 

Programação Funcional - Tipos, Classes e Cálculo Lambda
Felipe Daniel Dias dos Santos - 11711ECP004
Graduação em Engenharia de Computação - Faculdade de Engenharia Elétrica - Universidade Federal de Uberlândia

-}

--Exercício 2

data NomeP = Nome String deriving (Show, Eq, Ord)
data SobreNomeP = SobreNome String deriving (Show, Eq, Ord)
type NomeCompleto = (NomeP, SobreNomeP)

--Função que verifica se dois nomes n1 e n2 são iguais
compara :: NomeCompleto -> NomeCompleto -> Bool
compara n1 n2 = n1 == n2

{-

Teste 1: compara (Nome "Ana", SobreNome "Lima") (Nome "Caio", SobreNome "Silva").
Resultado: False.
Observação: Claramente, os dois nomes não são iguais.

Teste 2: (Nome "Ana", SobreNome "Lima") == (Nome "Caio", SobreNome "Silva").
Resultado: False.
Observação: Novamente, os dois nomes não são iguais.

Teste 3: (Nome "Cris", SobreNome "Dias") > (Nome "Cris", SobreNome "Dias").
Resultado: False.
Observação: Como os nomes são exatamente iguais, o primeiro não pode ser maior que o segundo. Uma variável x do tipo NomeCompleto 
é maior que outra variável de mesmo tipo y somente se o primeiro nome de x for maior que o primeiro nome de y, isto é, aparecer 
antes em uma ordenação alfabética. 

-}

--Exercício 3

data Exp a = Val a 
           | Neg (Exp a)
           | Add (Exp a) (Exp a)
           | Sub (Exp a) (Exp a)
           | Mul (Exp a) (Exp a)
           | Div (Exp a) (Exp a)

avalia :: Fractional a => Exp a -> a
avalia (Val x) = x
avalia (Neg exp) = -(avalia exp)
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) / (avalia exp2)

{-

Teste 1: avalia (Add (Val 1) (Mul (Val 2) (Val 3))).
Resultado: 7.0.
Observação: A função avalia recebe o argumento (Add (Val 1) (Mul (Val 2) (Val 3))) e resolve a expressão 
Add (Val 1) (Mul (Val 2) (Val 3)), que resulta em (Val 1) + (Mul (Val 2) (Val 3)). Após, cada argumento é avaliado e resolvido: 
Val 1 resula em 1 e (Mul (Val 2) (Val 3)) resulta em (Val 2) * (Val 3). Nesse momento, os dois argumentos de Mul são avaliados e 
Val 2 resulta em 2 e Val 3 resulta em 3. Assim, temos que (Val 2) * (Val 3) = 2 * 3. Logo, a função tem a seguinte expressão 
resultante: 1 + 2 * 3. Finalmente, a função resolve a expressão aritmética resultante e retorna o resultado 7. O passo a passo 
da execução é:

avalia (Add (Val 1) (Mul (Val 2) (Val 3))) -> (Val 1) + (Mul (Val 2) (Val 3)) -> 1 + (Mul (Val 2) (Val 3)) -> 1 + ((Val 2) * (Val 3))
-> 1 + (2 * (Val 3)) -> 1 + (2 * 3) -> 7.0.

Teste 2: avalia (Sub (Val 1) (Div (Val 8) (Val 4))).
Resultado: -1.0.
Observação: O passo a passo da execução é:

avalia (Sub (Val 1) (Div (Val 8) (Val 4))) -> (Val 1) - (Div (Val 8) (Val 4)) -> 1 - (Div (Val 8) (Val 4)) -> 1 - ((Val 8) / (Val 4))
-> 1 - (8 / (Val 4)) -> 1 - (8 / 4) -> -1.0.

-}

--Exercício 4

data LL = Latitude Int Int Int | Longitude Int Int Int deriving (Eq)

instance Show LL where 
    show(Latitude a b c) = "Lat " ++ show a ++ "°" ++ show b ++ "'" ++ show c ++ "''"
    show(Longitude a b c) = "Long " ++ show a ++ "°" ++ show b ++ "'" ++ show c ++ "''"
    
type PosicaoLocal = (String, LL, LL)
type Cidades = [PosicaoLocal]

--Exercício 4.A

c1, c2 :: PosicaoLocal
c1 = ("Brasilia", Latitude (-15) 46 47, Longitude 47 55 47)
c2 = ("Uberlandia", Latitude (-18) 55 07, Longitude 48 16 38)

eLat :: PosicaoLocal -> (String, LL)
eLat (p, (Latitude a b c), (Longitude x y z)) = (p, (Latitude a b c))

{-

As variáveis c1 e c2 são definidas com o tipo PosicaoLocal e são atribuídas a elas os seguintes valores:
c1 = ("Brasilia", Latitude (-15) 46 47, Longitude 47 55 47)
c2 = ("Uberlandia", Latitude (-18) 55 07, Longitude 48 16 38)

A função eLat retorna as informações de nome e latitude de um determinado valor do tipo PosicaoLocal, omitindo os informações sobre
longitude.

Teste 1: eLat c1.
Resultado: ("Brasilia", Lat -15°46'47'').
Observação: O retorno da função eLat para o parâmetro c1 é seu nome e sua latitude.

Teste 2: eLat ("Torres", Latitude (-29) 20 07, Longitude 49 43 37).
Resultado: ("Brasilia", Lat -15°46'47'').
Observação: Idem ao teste anterior, porém com um parâmetro distinto.

-}

--Exercício 4.B

--Função que calcula a coordenada geográfica em graus de uma cidade
calculaPosicao :: LL -> Float
calculaPosicao (Longitude x y z) = calculaPosicao (Latitude x y z) 
calculaPosicao (Latitude x y z) | x1 < 0 = x1 - y1 / 60 - z1 / 3600 
                                | otherwise = x1 + y1 / 60 + z1 / 3600 where
                                    x1 = fromIntegral(x)
                                    y1 = fromIntegral(y)
                                    z1 = fromIntegral(z)

--Função que verifica se uma cidade c1 está ao norte de uma cidade c2
norteDe :: PosicaoLocal -> PosicaoLocal -> Bool
norteDe c1 c2 = calculaPosicao latc1 > calculaPosicao latc2 where
    latc1 = snd $ eLat c1
    latc2 = snd $ eLat c2
       
{-

Teste: norteDe c1 c2.
Resultado: True.
Observação: De fato, a cidade de Brasília está ao norte da cidade de Uberlândia.

-}

--Exercício 4.C

lcidades :: Cidades
lcidades = [("Rio Branco", Latitude 09 58 29, Longitude 67 48 36), ("Brasilia", Latitude (-15) 46 47, Longitude 47 55 47), ("Torres", Latitude (-29) 20 07, Longitude 49 43 37), ("Joao Pessoa", Latitude (-07) 06 54, Longitude 34 51 47), ("Uberlandia", Latitude (-18) 55 07, Longitude 48 16 38)]

--Função que retorna a quantidade de cidades de uma lista que estão abaixo da linha do equador
abaixoEquador :: Cidades -> Int
abaixoEquador cs = length a where
    a = [c | c <- cs, norteDe equador c]
    equador = ("Equador", Latitude 0 0 0, Longitude 0 0 0)
    
--Função que retorna uma lista contendo os nomes das cidades que estão entre 40 e 50 graus de longitude
entre40e50 :: Cidades -> [String]
entre40e50 cs = a where
    a = [c | (c, _, long) <- cs, calculaPosicao long > 40 && calculaPosicao long < 50]

{-

Teste 1: abaixoEquador lcidades.
Resultado: 4.
Observação: De fato, das cidades fornecidas, somente Brasília, Torres, João Pessoa e Uberlândia estão abaixo da linha Equador.

Teste 2: entre40e50 lcidades.
Resultado: ["Brasilia","Torres","Uberlandia"].
Observação: De fato, das cidades fornecidas, somente Brasília, Torres e Uberlândia estão entre 40 e 50 graus em termos de longitude.

-}

--Exercício 5

data Talvez a = Valor a | Nada deriving (Show)

divisaoSegura :: Float -> Float -> Talvez Float
divisaoSegura x y = if y == 0 then Nada else Valor (x/y)

{-

O tipo Talvez possui dois valores possíveis: Nada ou Valor a, onde a é algum valor fornecido. Dependendo da definição das funções
que utilizam o tipo Talvez, qualquer um desses valores pode ser escolhido.

A função divisaoSegura é definida tendo como parâmetro dois valores x e y do tipo Float e retorno do tipo Talvez. Na chamada da 
função, primeiramente é analisado se o denominador y é igual a zero. Se sim, o valor Nada é retornado, pois divisão por zero é 
indefinida. Caso contrário, o valor Valor (x/y) é retornado, pois a divisão por qualquer número distinto de zero é definida e 
possui um valor númerico associado.

Teste 1: divisaoSegura 5 0.
Resultado: Nada.
Observação: A expressão 5 / 0 possui denominador igual a 0. Portanto, o valor Nada é retornado.

Teste 2: divisaoSegura 5 4.
Resultado: 1.25.
Observação: A expressão 5 / 4 possui denominador diferente de 0. Portanto, o valor Valor (5/4) = Valor 1.25 é retornado.

-}

--Exercício 6

addPares :: [(Int, Int)] -> [Int]
addPares lista = [m + n | (m, n) <- lista ]

--Exercício 6.A

addParesT :: [(Int, Int)] -> [Int]
addParesT ts = [x + y | (x, y) <- ts, x < y]

{-

Teste: addParesT [(2, 3), (2, 1), (3, 4)].
Resultado: [5, 7].
Observação: Somente as tuplas (2, 3) e (3, 4) são somadas.

-}

--Exercício 6.B

addParesNova :: [(Int, Int)] -> [Int]
addParesNova [] = []
addParesNova (t:ts) = (\(x, y) -> (x + y):addParesNova ts) t

{-

Teste: addParesNova [(2, 3), (2, 1), (3, 4)].
Resultado: [5, 3, 7].
Observação: Todas as tuplas são somadas.

-}

--Exercício 6.C

addParesTNova :: [(Int, Int)] -> [Int]
addParesTNova ts = map (\(x, y) -> x + y) $ filter (uncurry(<)) ts

{-

Teste: addParesTNova [(2, 3), (2, 1), (3, 4)].
Resultado: [5, 7].
Observação: Somente as tuplas (2, 3) e (3, 4) são somadas.

-}

--Execício 7

mp f [] ys = []
mp f xs [] = []
mp f (x:xs) (y:ys) = f x y : mp f xs ys

{-

A função mp tem como parâmetros uma função f e duas listas x e y e retorna uma lista s tal que seu i-ésimo elemento é igual ao 
resultado da função f aplicada nos i-ésimos elementos das listas x e y, isto é, s = [f x1 y1, f x2 y2, ..., f xn yn], onde xi é o 
i-ésimo elemento da lista x, yi é o i-ésimo elemento da lista y e n = min(comprimento x, comprimento y).

Teste 1: mp (**) [1, 2, 3] [4, 5, 6].
Resultado: [1.0, 32.0, 729.0].
Observação: A lista resultante s é tal que a posição i é igual a xi**yi, para 0 < i < 4.

Teste 2: mp (mod) [25, 8, 125] [5, 5, 5].
Resultado: [0, 3, 0].
Observação: A lista resultante s é tal que a posição i é igual a xi mod yi, para 0 < i < 4.

-}

--Exercício 8

--Função que retorna a soma dos quadrados dos números naturais de 1 a n
somaQuad :: Int -> Int
somaQuad n | n <= 0 = error "Deve ser inserido um número maior que 0"
           | otherwise = foldr1 ((\x -> \y -> x + y)) (map (^ 2) [1..n])

{-

Teste 1: somaQuad (-2).
Resultado: "Deve ser inserido um número maior que 0".
Observação: A função é definida para parâmetros positivos e inteiros.

Teste 2: somaQuad 1.
Resultado: 1.
Observação: 1^2 = 55.

Teste 3: somaQuad 5.
Resultado: 55.
Observação: 1^2 + 2^2 + 3^2 + 4^2 + 5^2 = 55.

-}

--Exercício 9

--Função que retorna a soma dos quadrados dos números positivos de uma lista xs
somaQuadPos :: [Int] -> Int
somaQuadPos xs | null xs = error "A lista deve ser não vazia."
               | otherwise = foldr1 ((\x -> \y -> x + y)) (map (^ 2) (filter (>= 0) xs))

{-

Teste 1: somaQuadPos [2, 0, -2, 6, -7, 4].
Resultado: 56.
Observação: 2^2 + 0^2 + 6^2 + 4^2 = 56.

Teste 2: somaQuadPos [].
Resultado: "A lista deve ser não vazia".
Observação: A função não é definida para uma lista vazia.

Teste 3: somaQuadPos [-2, 0, -2, -6, -7, 4].
Resultado: 16.
Observação: 0^2 + 4^2 = 16.

-}

--Exercício 10

fun x = [x]
misterio xs = foldr (++) [] (map fun xs)

{-

A execução da função misterio com um argumento xs tem dois passos: mapear os elementos da lista xs em função de fun, resultando em 
uma lista de listas, onde cada lista é um elemento de xs; e aplicar a função de concatenação (++) em cada lista do resultado 
anterior à lista vazia. Isso resulta na concatenação das sublistas de xs, onde cada sublista é um elemento de xs, que, no fim, 
resulta na própria lista.

Teste: misterio [1, 2, 3, 4].
Resultado: [1, 2, 3, 4].
Observação: O passo a passo da execução é:

misterio [1, 2, 3, 4] -> foldr (++) [] (map fun [1, 2, 3, 4]) -> foldr (++) [] [[1], [2], [3], [4]] 
-> foldr (++) ([4] ++ []) [[1], [2], [3]] -> foldr (++) ([4]) [[1], [2], [3]] -> foldr (++) ([3] ++ [4]) [[1], [2]] 
-> foldr (++) ([3, 4]) [[1], [2]] -> foldr (++) ([2] ++ [3, 4]) [[1]] -> foldr (++) ([2, 3, 4]) [[1]] 
-> foldr (++) ([1] ++ [2, 3, 4]) [] -> foldr (++) ([1, 2, 3, 4]) [] -> [1, 2, 3, 4]

-}
