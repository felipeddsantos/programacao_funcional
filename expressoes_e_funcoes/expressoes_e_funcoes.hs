{- 

Programação Funcional - Expressões e Funções
Felipe Daniel Dias dos Santos - 11711ECP004
Graduação em Engenharia de Computação - Faculdade de Engenharia Elétrica - Universidade Federal de Uberlândia

-}

--Exercício 5

--Função que calcula a raíz de uma equação do primeiro grau ax + b = 0
raiz1 :: Float -> Float -> Float
raiz1 a b = -b / a

--Função para o cálculo de delta da equação de Bháskara
delta :: Float -> Float -> Float -> Float
delta a b c = b * b - 4 * a * c

--Função que calcula as raízes de uma equação do segundo grau ax ^ 2 + bx + c = 0
raiz2 :: Float -> Float -> Float -> (Float, Float)
raiz2 a b c = (x1, x2) where
    d = delta a b c
    x1 = (-b + sqrt d) / (2 * a) 
    x2 = (-b - sqrt d) / (2 * a)

--Exercício 7

--Função para o cálculo do mdc de dois valores com o Algoritmo de Euclides
mdc2 :: Int -> Int -> Int
mdc2 a 0 = a
mdc2 a b = mdc2 b (mod a b)

--Função para o cálculo do mdc de três valores com o Algoritmo de Euclides e a função auxiliar "mdc2"
--considerando o fato de que mdc(a, b, c) = mdc(mdc(a, b), c) 
mdc :: Int -> Int -> Int -> Int
mdc a b 0 = mdc2 a b
mdc a b c = mdc2 c (mod (mdc2 a b) c)

--Exercício 6

{-
 
Sabemos que mmc(a, b, c) = mmc(mmc(a, b), c) e mmc(a, b) = ab / (mdc(a, b)), portanto, concluimos que
mmc(a, b, c) = mmc(a, b)c / (mdc(mmc(a, b) ,c)). A estratégia do programa será utilizar a função "mdc_2"
para o cálculo do mdc de dois valores para auxiliar no cálculo do mmc de dois valores que, por sua vez,
será utilizado para o cálculo do mmc de três valores

-}

--Função para o cálculo do mmc de dois valores com a função "mdc2" 
mmc2 :: Int -> Int -> Int
mmc2 a b = div (a * b) (mdc2 a b) 

--Função para o cálculo do mmc de três valores com as funções auxiliares "mmc2" e "mdc2"
mmc :: Int -> Int -> Int -> Int
mmc a b c = div (c * mmc2 a b) (mdc2 (mmc2 a b) c)

--Exercício 8

{-
 
A sequência de Fibonacci é caracterizada pelo fato de que cada elemento é a soma dos dois anteriores,
exceto pelos dois primeiros, que são, respectivamente 0 e 1. Utilizando esse fato, a função será
definida recursivamente.

-}

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

--Exercício 9

{- 

Em relação a classificação de um ano, sabemos que qualquer ano bissexto é divisível por 400 
ou divisível por 4 e não divisível por 100. Qualquer ano que não se enquadre nessas regras, não é 
bissexto. Além disso, o ano deve ser um valor natural.

-}

--Função para analisar se um ano é bissexto
bissexto :: Int -> Bool
bissexto ano | mod ano 400 == 0 && ano > 0 = True
             | mod ano 4 == 0 && mod ano 100 /= 0 && ano > 0 = True
             | otherwise = False

--Exercício 10

{-
 
Para que uma data seja válida, é necessário que:

*O ano seja um número maior que 0;
*O mês seja um número compreendido entre 1 e 12; 
*Os dias dos meses 1, 3, 5, 7, 8, 10 e 12 estejam compreendidos entre 1 e 31;
*Os dias dos meses 4, 6, 9 e 11 estejam compreendidos entre 1 e 30;
*Os dias do mês 2, caso o ano seja bissexto, estejam compreendidos entre 1 e 29;
*Os dias do mês 2, caso o ano não seja bissexto, estejam compreendidos entre 1 e 28;

-}

--Função para analisar se um determinado dia é válido considerando o mês e o ano
diaValido :: Int -> Int -> Int -> Bool
diaValido dia mes ano | (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) && (dia >= 1 && dia <= 31) = True
                      | (mes == 4 || mes == 6 || mes == 9 || mes == 11) && (dia >= 1 && dia <= 30) = True
                      | mes == 2 && bissexto ano && dia >= 1 && dia <= 29 = True  
                      | mes == 2 && not (bissexto ano) && dia >= 1 && dia <= 28 = True       
                      | otherwise = False  

--Função para analisar se uma determinada data é válida cosiderando a combinação de dia, mês e ano
dataValida :: Int -> Int -> Int -> Bool
dataValida dia mes ano = ano > 0 && diaValido dia mes ano
