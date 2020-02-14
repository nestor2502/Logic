{-
- Lógica computacional 2020-2
- Practica 1
- Alumno: José David Ramírez Rojas
- Número de cuenta: 316184924
-Correo: josedavidrr@ciencias.unam.mx
- Alumno: Néstor Semer Vázquez Cordero
- Número de cuenta:  
-Correo: nestor2502@ciencias.unam.mx
-}

module Practica1 where

-- Tipos definidos

--Definir el tipo Complejo
type Complejo = (Float,Float)


--Forma para definir el tipo de los números naturales
--de manera recursiva iniciando desde el cero.
data Nat = Cero | Suc Nat deriving(Show,Eq)


--Definición recursiva de listas, la lista más
--pequeña es la lista vacía representada por la palabra Nula
data Lista a = Nula | Cons a (Lista a) deriving(Show,Eq)


--Definición recursiva de árboles, el árbol más pequeño es 
--el árbol vacío.
data Arbol = Vacio | Nodo Arbol Int Arbol deriving(Show,Eq)


-- Funciones principales

--Funcion puntoMedio que dados dos puntos en el plano encuentra el punto medio entre los dos.
--Ejemplo
--Prelude>puntoMedio (-1,2) (7,6)
--(3.0,4.0)
puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x,y) (w,z) = ((x+w)/2,(y+z)/2)


--Función que dada una ecuación de segundo grado encuentra las raices de esta en una
--pareja ordenada
raices :: Float -> Float -> Float -> (Complejo,Complejo)
raices a b c= if r >=0
                then (((-b+sqrt(s))/q, 0),((-b-sqrt(s))/q, 0))
                else ((-(b)/q, sqrt(s)/q), (-b/q, -(sqrt(s))/q))
                where
                r= b^2-(4*a*c)
                s = -(r)
                q = 2*a

--Definir la función segmento tal que (segmento m n xs) es la lista de los
--elementos de xs comprendidos entre las posiciones m y n. Por ejemplo,
--segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
--segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
--segmento 5 3 [3,4,1,2,7,9,0] == []
--segmento :: Int -> Int -> [a] -> [a]
--segmento = error "Te toca"


--Definir la función extremos tal que (extremos n xs) es la lista formada
--por los n primeros elementos de xs y los n finales elementos de xs. Por ejemplo,
--extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]
extremos :: Int -> [a] -> [a]
extremos n x = if n*2 >= length x
               then x
               else take n x ++ reverse (take n (reverse x))

--Funcion que elimina un intervalo de una lista; dados dos números y una lista,
--elimina los elementos que se encuentren en el intervalo de esos dos numeros.
--Por ejemplo,
--dIntervalos 2 4 [1,2,3,4,5,6,7] == [1,5,6,7]
--dIntervalos :: Int -> Int -> [a] -> [a]
--dIntervalos = error "Te toca"


--Un número natural n se denomina abundante si es menor que la suma de sus divisores
--propios, sin el mismo. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.
--Definir la función numerosAbundantes tal que (numerosAbundantes n)
--es la lista de números abundantes menores o iguales que n. Por ejemplo,
--numerosAbundantes 50 == [12,18,20,24,30,36,40,42,48]
--numerosAbundantes :: Int -> [Int]
--numerosAbundantes n = [x | x<=n,esAbundante x]


--Definir la función que recibe una lista y regrese una lista tal que 
--la lista resultante contiene los mismos elementos que la original pero
--sin duplicados.
--Ejemplo:
--eliminaDuplicados [1,3,1,2,3,2,1] ; [1,3,2]
--eliminaDuplicados :: Eq a => [a] -> [a]
--eliminaDuplicados = error "Te toca"


--Se define el primitivo de un número como sigue:
--Dado un número natural n, multiplicamos todos sus dígitos, 
--repetimos este procedimiento hasta que quede un solo dígito al 
--cual llamamos primitivo de n. Por ejemplo, para 327 
--327 : 3 X 2 X 7 = 42 y 4 X 2 = 8. 
--Por lo tanto, el primitivo de 327 es 8.
--Definir la función dado un número nos regrese el primitivo. 
--Ejemplo:
--primitivo 327 ; 8
--primitivo :: Integer -> Integer
--primitivo = error "Te toca"


--Función que dadas dos listas y un Natural j, regresa una lista tal que, se encuentran 
--concatenados el i-ésimo elemento de la primer Lista con el i-ésimo elemento de la 
--segunda Lista; a partir del elemento j de cada una de las listas.
--Ejemplo:
--sipLis (Suc (Suc Cero)) (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nula))))) (Cons 7 (Cons 8 (Cons 9 Nula))) ==
--(Cons 2 (Cons 8 (Cons 3 (Cons 9 Nula))))
--sipLis :: Nat -> Lista a -> Lista a -> Lista a
--sipLis = error "Te toca


-- Funciones auxiliares en caso de tenerlas van aquí

--Función que obtiene si un número es abundante
--Ejemplo:
--esAbundante :: Int -> Bool
--esAbundante n = if 

--Función que obtiene los divisores de un número n
--Ejemplo:
divisores :: Int -> [Int]
divisores k = divisores' 1 k
              where divisores' n k | n*n > k = [k]
                                   | n*n == k = [n, k]
                                   | k `mod` n == 0 = (n:(k `div` n):resultado)
                                   | otherwise = resultado
    	                           where resultado = divisores' (n+1) k

--Dados dos números x,y determina si y divide a x
divisor :: Int -> Int -> Int
divisor x y = if x `rem` y == 0 
                then y
			    else 0

sumaDivisores :: Int -> Int
sumaDivisores 1 = 1
sumaDivisores x = auxSumaDivisores x (x-1)

auxSumaDivisores :: Int -> Int -> Int
auxSumaDivisores _ 1 = 1
auxSumaDivisores x y = divisor x y + auxSumaDivisores x (y-1)

esAbundante :: Int -> Bool
esAbundante x = sumaDivisores x > x

--Suerte y no olviden seguir los lineamientos de entrega.