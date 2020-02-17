{-
- Lógica computacional 2020-2
- Practica 1
- Alumno: José David Ramírez Rojas
- Número de cuenta: 316184924
-Correo: josedavidrr@ciencias.unam.mx
- Alumno: Néstor Semer Vázquez Cordero
- Número de cuenta: 316041625
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
raices :: Float -> Float -> Float -> (Complejo,Complejo)
raices a b c= if r >=0
                then (((-b+sqrt(r))/q, 0),((-b-sqrt(r))/q, 0))
                ---then ((r,0),(0,0))
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
segmento :: Int -> Int -> [a] -> [a]
segmento x y xs = drop (x-1) (take y xs)


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
dIntervalos :: Int -> Int -> [a] -> [a]
dIntervalos x y xs = (take (x-1) xs) ++ (drop (y) xs)


--Un número natural n se denomina abundante si es menor que la suma de sus divisores
--propios, sin el mismo. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.
--Definir la función numerosAbundantes tal que (numerosAbundantes n)
--es la lista de números abundantes menores o iguales que n. Por ejemplo,
--numerosAbundantes 50 == [12,18,20,24,30,36,40,42,48]
numerosAbundantes :: Int -> [Int]
numerosAbundantes n = [ x | x <-[12..n], sum (divisores x) > x ]--empieza desde 12 pues tenemos certeza que es el 1er número abundante


--Definir la función que recibe una lista y regrese una lista tal que 
--la lista resultante contiene los mismos elementos que la original pero
--sin duplicados.
--Ejemplo:
eliminaDuplicados :: Eq a => [a] -> [a]
eliminaDuplicados [] = []
eliminaDuplicados (x:xs) = if x `elem` xs
                            then eliminaDuplicados xs
                            else x: eliminaDuplicados xs


--Se define el primitivo de un número como sigue:
--Dado un número natural n, multiplicamos todos sus dígitos, 
--repetimos este procedimiento hasta que quede un solo dígito al 
--cual llamamos primitivo de n. Por ejemplo, para 327 
--327 : 3 X 2 X 7 = 42 y 4 X 2 = 8. 
--Por lo tanto, el primitivo de 327 es 8.
--Definir la función dado un número nos regrese el primitivo. 
--Ejemplo:
--primitivo 327 ; 8
primitivo :: Integer -> Integer
primitivo n = if n <10 
              then n
              else primitivo (product (separaDigitos n))


--Función que dadas dos listas y un Natural j, regresa una lista tal que, se encuentran 
--concatenados el i-ésimo elemento de la primer Lista con el i-ésimo elemento de la 
--segunda Lista; a partir del elemento j de cada una de las listas.
--Ejemplo:
--sipLis (Suc (Suc Cero)) (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nula))))) (Cons 7 (Cons 8 (Cons 9 Nula))) ==
--(Cons 2 (Cons 8 (Cons 3 (Cons 9 Nula))))
sipLis :: Nat -> Lista a -> Lista a -> Lista a
sipLis Cero Nula ys = Nula
sipLis Cero xs Nula = Nula
sipLis Cero (Cons x xs) (Cons y ys) = Cons x (Cons y (sipLis Cero xs ys) ) 
sipLis (Suc Cero) (Cons x xs) (Cons y ys) = Cons x (Cons y (sipLis Cero xs ys) ) 
sipLis (Suc m) (Cons x xs) (Cons y ys) = sipLis(m) xs ys

--Función la cual convierte un  árbol en una lista haciendo el recorrido en preorden.
--Ejemplo: aplanaArbolPre arbol1 = [2,0,-1,1,4,3,5].
aplanaArbolPre :: Arbol -> [Int]
aplanaArbolPre Vacio = []
aplanaArbolPre t = [obtenerValorNodo (t)] ++ aplanaArbolPre (obtenerArbolIzq(t))
    ++ aplanaArbolPre (obtenerArbolDer(t))

-- Funciones auxiliares en caso de tenerlas van aquí

divisores :: Int -> [Int]
divisores n = [x | x <- [1..(n-1)], n `rem` x == 0]

--Separa los digitos de un número x y los acomoda en una lista
separaDigitos :: Integer -> [Integer]
separaDigitos 0 = []
separaDigitos x = separaDigitos (x `div` 10) ++ [x `mod` 10]

obtenerValorNodo:: Arbol -> Int
obtenerValorNodo (Nodo _ x _) = x

obtenerArbolIzq:: Arbol -> Arbol
obtenerArbolIzq (Nodo t _ _) = t

obtenerArbolDer:: Arbol -> Arbol
obtenerArbolDer (Nodo _ _ t) = t
