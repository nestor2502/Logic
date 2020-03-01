{-
- Lógica computacional 2020-2
- Practica 2
- Alumno: José David Ramírez Rojas
- Número de cuenta: 316184924
- Correo: josedavidrr@ciencias.unam.mx
- Alumno: Néstor Semer Vázquez Cordero
- Número de cuenta: 316041625
- Correo: nestor2502@ciencias.unam.mx
-}

module Practica2 where

import Data.List

-- ---------------------------------------------------------------------
-- Definimos los siguientes tipos de datos:
-- Prop para representar las fórmulas proposicionales usando los
-- constructores T, F, Var, Neg, Conj, Disy, Impl y Equi para las fórmulas
-- atómicas, negaciones, conjunciones, implicaciones y equivalencias,
-- respectivamente.
-- ---------------------------------------------------------------------

data Prop = T | F | Var String | Neg Prop | Conj Prop Prop | Disy Prop Prop | Impl Prop Prop | Equi Prop Prop deriving Eq

type Estado = [String]

instance Show Prop where
         show T = "Verdadero"
         show F = "Falso"
         show (Var p) = p
         show (Neg p) = "¬" ++ show p
         show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
         show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
         show (Impl p q) = "(" ++ show p ++ " ⟶  " ++ show q ++ ")"
         show (Equi p q) = "(" ++ show p ++ " ⟷  " ++ show q ++ ")"


-- ---------------------------------------------------------------------
-- Definimos las siguientes fórmulas proposicionales
-- como variables atómicas: p, q, r, s, t, u.
-- ---------------------------------------------------------------------
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"


-- ---------------------------------------------------------------------
-- Símbolos proposicionales de una fórmula --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
-- variables :: Prop -> [String]
-- tal que (variables f) es el conjunto formado por todos los
-- símbolos proposicionales que aparecen en f. Por ejemplo,
-- >variables (Impl (Conj (Var "p") (Var "q")) (Var "p"))
-- ["p","q"]
-- >variables Conj (Var "q") (Disy (Var "r") (Var "p")) 
-- ["q","r","p"]
-- ---------------------------------------------------------------------

variables :: Prop -> Estado
variables phi = case phi of
	T -> []
	F -> []
	Var alpha -> [alpha]
	Neg alpha -> nub (variables alpha)
	Conj alpha beta -> nub(variables alpha ++ variables beta)
	Disy alpha beta -> nub(variables alpha ++ variables beta)
	Impl alpha beta -> nub(variables alpha ++ variables beta)
	Equi alpha beta -> nub(variables alpha ++ variables beta)

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
-- conjPotencia :: [a] -> [[a]]
-- tal que (conjPotencia x) es la lista de todos los subconjuntos de x.
-- Por ejmplo,
-- >conjPotencia [1,2]
-- [[]; [2]; [1]; [1; 2]]
-- >conjPotencia []
-- [[]]
-- >conjPotencia "abc"
-- ["abc","ab","ac","a","bc","b","c",""]
-- ---------------------------------------------------------------------

conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [x:ys | ys <- conjPotencia xs] ++ conjPotencia xs

-- ---------------------------------------------------------------------
-- Interpretaciones --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función
-- interpretacion :: Prop -> Estado -> Bool
-- tal que (interpretacion f e) es la interpretación de f en e. Por ejemplo,
-- interpretacion Conj (Var "q") (Disy (Var "r") (Var "p")) ["p"]
-- False 
-- >interpretacion Conj (Var "q") (Disy (Var "r") (Var "p")) ["p","q"]
-- True
-- ---------------------------------------------------------------------

interpretacion :: Prop -> Estado -> Bool
interpretacion phi alpha = case phi of
	T -> True
	F -> False
	Var beta -> elem beta alpha
	Neg beta -> not(interpretacion beta alpha)
	Conj beta gamma -> (interpretacion beta alpha) && (interpretacion gamma alpha)
	Disy beta gamma -> (interpretacion beta alpha) || (interpretacion gamma alpha)
	Impl beta gamma -> (not(interpretacion beta alpha)) || (interpretacion gamma alpha)
	Equi beta gamma -> (interpretacion (Impl beta gamma) alpha) && (interpretacion (Impl gamma beta) alpha)


-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir una función que dada una fórmula proposicional,
-- la función devuelve todos los estados con los que podemos evaluar
-- la fórmula. Por ejemplo,
-- >estadosPosibles Disy (Var "q") (Conj (Var "r") (Var "q"))
-- [[],["q"]; ["r"]; ["q","r"]]
-- ---------------------------------------------------------------------

estadosPosibles :: Prop -> [Estado]
estadosPosibles phi = conjPotencia (variables phi)


-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir una función que dada una fórmula proposicional,
-- nos diga si es una tautología. Por ejemplo,
-- >tautologia Disy (Var "p") (Neg (Var "p"))
-- True
-- >tautologia Disy (Var "q") (Var "r")
-- False
-- ---------------------------------------------------------------------

tautologia :: Prop -> Bool
tautologia phi = and[interpretacion phi xs | xs <- estadosPosibles(phi)]


-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir una función que dada una fórmula proposicional,
-- nos diga si es una contradicción. Por ejemplo,
-- >contradiccion Disy (Var "p") (Neg (Var "p"))
-- False
-- ---------------------------------------------------------------------

contradiccion :: Prop -> Bool
contradiccion phi = and[not (interpretacion phi xs) | xs <- estadosPosibles(phi)]


-- ---------------------------------------------------------------------
-- Modelos --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 7: Definir una función que dada una interpretación y una 
-- fórmula proposicional, verifique si esta interpretación es un modelo.
-- Por ejemplo,
-- >esModelo ["r"] (Conj (Disy p q) (Disy (Neg q) r))
-- False
-- >esModelo ["p","r"] (Conj (Disy p q) (Disy (Neg q) r)) 
-- True
-- ---------------------------------------------------------------------

esModelo :: Estado -> Prop -> Bool
esModelo e phi = case phi of
	T -> True
	F -> False 
	Var alpha -> elem alpha e
	Neg alpha -> not(esModelo e alpha)
	Conj alpha beta -> (esModelo e alpha) && (esModelo e beta)
	Disy alpha beta -> (esModelo e alpha) || (esModelo e beta)
	Impl alpha beta -> not(esModelo e alpha) || (esModelo e beta)
	Equi alpha beta -> (esModelo e (Impl alpha beta)) && (esModelo e (Impl beta alpha))

-- ---------------------------------------------------------------------
-- Ejercicio 8: Definir una función que dada una fórmula proposicional
-- devuelve la lista de todos sus modelos; tal que (modelos f) es la 
-- lista de todas las interpretaciones de f que son modelo. Por ejemplo,
-- >modelos (Conj (Disy p q) (Disy (Neg q) r))
-- [["p","q","r"],["p","r"],["p"],["q","r"]]
-- ---------------------------------------------------------------------

modelos :: Prop -> [Estado]
modelos phi = [e | e <- estadosPosibles phi, esModelo e phi]

-- ---------------------------------------------------------------------
-- Ejercicio 9: Definir una función que dada una fórmula proposicional f
-- verifica si f es válida. Por ejemplo,
-- esValida (Impl p p)
-- True
-- esValida (Impl p q) 
-- False
-- esValida (Disy (Impl p q) (Impl q p))
-- True
-- ---------------------------------------------------------------------

esValida :: Prop -> Bool
esValida phi = tautologia (phi)

-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir una función que dada una fórmula proposicional f
-- verifica si f es insatisfacible. Por ejemplo,
-- esInsatisfacible (Conj p (Neg p)) 
-- True
-- esInsatisfacible (Conj (Impl p q) (Impl q r))
-- False
-- ---------------------------------------------------------------------

esInsatisfacible :: Prop -> Bool
esInsatisfacible phi = contradiccion (phi)

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir una función que dada una fórmula proposicional f
-- verifica si f es satisfacible. Por ejemplo,
-- esSatisfacible (Conj p (Neg p)) 
-- False
-- esSatisfacible (Conj (Impl p q) (Impl q r)) 
-- True
-- ---------------------------------------------------------------------

esSatisfacible :: Prop -> Bool
esSatisfacible phi = or [interpretacion phi xs | xs <- estadosPosibles (phi)]
