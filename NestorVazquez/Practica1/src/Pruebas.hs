--Tipos definidos

--Forma para definir el tipo de los números naturales
--de manera recursiva iniciando desde el cero.
data Nat = Cero | Suc Nat deriving(Show,Eq)


--Definición recursiva de listas, la lista más
--pequeña es la lista vacía representada por la palabra Nula
data Lista a = Nula | Cons a (Lista a) deriving(Show,Eq)

--Funciones principales

--Función recursiva para sumar dos naturales
--Recibe dos naturales y regresa un natural
--Ejemplo: 
--Main> sumaNat (Suc(Suc(Suc Cero))) (Suc(Suc(Suc(Suc(Suc(Suc Cero))))))
--Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Cero))))))))

type Complejo = (Float,Float)

--Problema 1
puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x,y) (w,z) = ((x+w)/2,(y+z)/2)

--Problema2
raices :: Float -> Float -> Float -> (Complejo,Complejo)
raices a b c= if r >=0
                then (((-b+sqrt(r))/q, 0),((-b-sqrt(r))/q, 0))
                ---then ((r,0),(0,0))
                else ((-(b)/q, sqrt(s)/q), (-b/q, -(sqrt(s))/q))
                where
                r= b^2-(4*a*c)
                s = -(r)
                q = 2*a

--Problema 3
segmento :: Int -> Int -> [a] -> [a]
segmento x y xs = drop (x-1) (take y xs)

--Problema 5 
dIntervalos :: Int -> Int -> [a] -> [a]
dIntervalos x y xs = (take (x-1) xs) ++ (drop (y) xs)

--Problema 7
eliminaDuplicados :: Eq a => [a] -> [a]
eliminaDuplicados listaNum | listaNum == [] = []
| any (== head listaNum) (tail listaNum) = eliminaDuplicados (tail listaNum)
| otherwise = (take 1 listaNum) ++ eliminaDuplicados (tail listaNum)
--eliminaDuplicados (x:xs) = if x `elem` xs
                           -- then eliminaDuplicados xs
                           -- else x: eliminaDuplicados xs

--Problema 9
sipLis :: Nat -> Lista a -> Lista a -> Lista a
sipLis Cero Nula ys = Nula
sipLis Cero xs Nula = Nula
sipLis Cero (Cons x xs) (Cons y ys) = Cons x (Cons y (sipLis Cero xs ys) ) 
sipLis (Suc Cero) (Cons x xs) (Cons y ys) = Cons x (Cons y (sipLis Cero xs ys) ) 
sipLis (Suc m) (Cons x xs) (Cons y ys) = sipLis(m) xs ys


