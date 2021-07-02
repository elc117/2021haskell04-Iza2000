-- Prática 04 de Haskell
-- Nome: Izabella M. Paulette

--1)A vacinação contra COVID19 no Brasil está acontecendo por grupos prioritários. As equipes responsáveis pelas ações de vacinação devem registrar em um sistema cada dose de vacina aplicada, classificando cada indivíduo em um dos grupos previstos. No caso de idosos, este grupo prioritário é organizado em 5 faixas etárias: de 60 a 64 anos, de 65 a 69 anos. de 70 a 74 anos, de 75 a 79 anos e de 80 anos ou mais. No sistema, essas faixas são identificadas, respectivamente, pelas siglas "IDO64", "IDO69", "IDO74", "IDO79" e "IDO80". Sabendo disso, crie uma função faixaIdoso :: Int -> String que receba uma idade e retorne o código da faixa correspondente. Caso a idade não se enquadre em nenhuma das faixas do grupo prioritário, o código será "ND" (não definido).
faixaIdoso :: Int -> String
faixaIdoso idade = if idade >= 60 && idade <= 64 then "IDO64"
                   else if idade >= 65 && idade <= 69 then "IDO69"
                   else if idade >= 70 && idade <= 74 then "IDO74"
                   else if idade >= 75 && idade <= 79 then "IDO79"
                   else if idade >= 80 then "IDO80"
                   else "ND"

--2)Usando list comprehension, crie uma função classifIdosos :: [(String,Int)] -> [(String,Int,String)] que receba uma lista de tuplas contendo nome e idade, e retorne uma lista de tuplas com nome, idade e o código da faixa correspondente (faixaIdoso). Por exemplo:
-- > classifIdosos [("joao",65), ("maria", 64)]
--[("joao",65,"IDO69"),("maria",64,"IDO64")]
classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos lisTupla = [(nome, idade, faixaIdoso idade) | (nome, idade) <- lisTupla]

--3)Resolva o exercício anterior com função de alta ordem, sem usar list comprehension. O novo nome da função será classifIdosos'.
classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' lisTupla' = map (\(nome,idade) -> (nome,idade,faixaIdoso idade)) lisTupla'

--4)Suponha que uma cor seja representada por uma tupla (Int,Int,Int), contendo valores (R=red,G=Green,B=blue). Considerando isso, crie uma função strColor :: (Int,Int,Int) -> String que receba uma tupla representando uma cor (R=red,G=Green,B=blue) e retorne uma string no formato "rgb(R,G,B)". Por exemplo:
-- > strColor (90,0,35)
--"rgb(90,0,35)"
strColor :: (Int,Int,Int) -> String
strColor (r,g,b) = "rgb" ++ "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

--5)Suponha que um círculo seja representado por uma tupla (Int,Int,Int), contendo respectivamente as coordenadas x e y de seu centro, seguidas de seu raio. Considerando isso, crie uma função genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)] que receba um número N, um ponto (cx,cy) e um raio R, e gere uma sequência de N círculos de raio R alinhados horizontalmente com um primeiro círculo centrado em (cx,cy). Você pode decidir qual será a distância entre eles. Por exemplo:
-- > genCircs 5 (10,10) 2
--[(10,10,2),(14,10,2),(18,10,2),(22,10,2),(26,10,2)]
genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (px,py) r = take n [(x,py,r) | x <- (iterate (4+) px)]

--6)Suponha novamente que uma cor seja representada por uma tupla (Int,Int,Int), contendo valores (R=red,G=Green,B=blue). Sabendo disso, crie uma função genReds :: Int -> [(Int,Int.Int)] que receba um número N e gere uma lista com N tons de vermelho calculados (não enumere cada um dos valores literalmente com números "hard-coded"). Você pode repetir valores, se desejar. Abaixo está um exemplo de uso dessa função:
-- > genReds 5
--[(80,0,0),(90,0,0),(100,0,0),(110,0,0),(120,0,0)]
genReds :: Int -> [(Int,Int,Int)]
genReds num = take num [(x,0,0) | x <- [1,11..255], x > num]