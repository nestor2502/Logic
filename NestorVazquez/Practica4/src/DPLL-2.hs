{-
- Logica Conmputacional 2020-2 
- Práctica 4, Implementación del algoritmo dpll.
- Creador: Pedro Juan Salvador Sánchez Pérez
-}

module DPLL where

import LProp
import Data.List

type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)
type AuxSplit = [Solucion]

-- Seccion de funciones para la regla de la clausula unitaria
unit :: Solucion -> Solucion 
unit ([],[]) = ([],[])
unit (m,f) = ( m ++ agrega_unitaria_de_formula f, delete (agrega_unitaria_de_formula f) f)


agrega_unitaria_de_formula :: Formula -> Modelo 
agrega_unitaria_de_formula [] = []                          
agrega_unitaria_de_formula (x:xs) = if length (x) == 1
                                    then
                                        agrega_unitaria_de_clau x
                                    else 
                                    	agrega_unitaria_de_formula xs 

agrega_unitaria_de_clau :: Clausula -> Modelo 
agrega_unitaria_de_clau [c] = [agrega_unitaria_de_literal c]
agrega_unitaria_de_clau (x:xs) = []

agrega_unitaria_de_literal :: Literal -> Literal 
agrega_unitaria_de_literal l = case l of 
                                 V beta -> V beta
                                 Neg beta -> Neg (beta)


--  Seccion de funciones para la regla de eliminacion

elim :: Solucion -> Solucion
elim ([],[])= ([],[])
elim(m, f) = (m,elimi_formula_conj m f)

-- si una si alguna literal del modelo se encuentra en alguna clausula de la formula, elimina la clausula

elimi_formula_conj ::Modelo -> Formula -> Formula
elimi_formula_conj [x] f = elimi_formula x f
elimi_formula_conj (x:xs) f =  elimi_formula_conj xs (elimi_formula x f)             

-- Si una literal se encuentra en una clausula de una formula elimina la clausula

elimi_formula :: Literal -> Formula -> Formula
elimi_formula l [x] = [pertenece l x]
elimi_formula l (x:xs) = elimina_vacia(pertenece l x:reduce_no_nec l xs)

--si se encuentra una literal en una clausula elimina la clausula
	
pertenece :: Literal -> Clausula-> Clausula
pertenece l xs=  if elem l xs
					then []
					else xs

--Funcion que elimina a la lista vacia de una formula
elimina_vacia :: Formula -> Formula
elimina_vacia [] = []
elimina_vacia xs = [x | x<- xs, x/=[]]

-- Seccion de funciones para la regla de reduccion

red :: Solucion -> Solucion  --NO
red ([],[])= ([],[])
red(m, f) = (m,reduce_formula_conj m f)

--Funcion auxiliar que verifica las variables complementarias de un modelo en una formula

reduce_formula_conj ::Modelo -> Formula -> Formula
reduce_formula_conj [x] f = reduce_no_nec x f
reduce_formula_conj (x:xs) f =  reduce_formula_conj xs (elimi_formula x f) 

--funcion auxiliar que elimina las literales complementarias de una formula

reduce_no_nec :: Literal -> Formula -> Formula
reduce_no_nec l [x] =  [red1 l x]
reduce_no_nec l (x:xs) = red1 l x : reduce_no_nec l xs

--auxiliar 1, verifique si se encuentra una literal complementaria y la elimina
red1 :: Literal -> Clausula -> Clausula
red1 l xs =  [ x | x<- xs , (aux_com l x)==False]

-- Si una literal es complementaria de otra devuelve true

aux_com :: Literal -> Literal -> Bool
aux_com l m = if l == Neg m || m == Neg l 
			then True
			else False


-- Seccion de funciones para la regla de separacion

split :: Solucion -> AuxSplit
split (m,f) = [(seleccionar_literal_de_for f :m, f), (Neg(seleccionar_literal_de_for f): m,f)]

--auxiliar 1 que selecciona una literal de una formula

seleccionar_literal_de_for :: Formula -> Literal
seleccionar_literal_de_for [x] = seleccionar_literal_de_clau x
seleccionar_literal_de_for (x:xs) = seleccionar_literal_de_clau x


--Auxiliar 2 que selecciona una literal de una clausula

seleccionar_literal_de_clau :: Clausula -> Literal
seleccionar_literal_de_clau [x] = x
seleccionar_literal_de_clau (x:xs) = x

-- Seccion de funciones para la regla de conflicto

conflict :: Solucion -> Bool  --NO
conflict (m,f) = if elem [] f
                 then True
                 else False

-- Seccion de funciones para la regla de exito

success :: Solucion -> Bool
success (m,f) = if f == []
					then True
					else False

-- Sección de funciones auxiliares de dpllsearch

-- Devuelve si quedan literales dentro de la fórmula del lado derecho
no_quedan_unit :: Solucion -> Bool
no_quedan_unit (m,f) = if unit ([],f) == ([],f)	
                       then True
                       else False

--Devuelve si el modelo de solución actual está vacío
modelo_vacio :: Solucion -> Bool
modelo_vacio (m,f) = if m == []	
                       then True
                       else False
     
-- Devuelve si quedan literales complementarias dentro de la fórmula del lado derecho
no_quedan_lit_complementarias :: Solucion -> Bool
no_quedan_lit_complementarias (m,f) = if red (m,f) == (m,f)	
                                        then True
                                        else False

-- Devuelve si quedan literales complementarias dentro de la fórmula del lado derecho
no_quedan_lit_en_clausulas_de_form :: Solucion -> Bool
no_quedan_lit_en_clausulas_de_form (m,f) = if elim (m,f) == (m,f)
                                        then True
                                        else False   

dpll_splitted :: AuxSplit -> Solucion
dpll_splitted [x] = dpll x
dpll_splitted (x:xs) = if conflict (dpll (x)) == False
	                   then dpll x
	                    else error "kk"

-- Seccion de las funciones principales de DPLL

dpllsearch :: Solucion-> Solucion  
dpllsearch (m, []) = (m, [])
dpllsearch (m,f) = if modelo_vacio (m,f) == True
	               then if no_quedan_unit (m,f) == True
	                  	then dpll_splitted (split (m,f))
	                  	else dpll (unit (m,f))
	               else if no_quedan_lit_complementarias (m,f) == False --error actual al verif esta condicion
	                  	then dpll (red (m,f))
	                  	else if no_quedan_lit_en_clausulas_de_form (m,f) == False 
	                  	     then dpll (elim (m,f)) 
                             else if no_quedan_unit (m,f) == False
	                              then dpll (unit (m,f))
	                              else dpll_splitted (split (m,f))

dpll :: Solucion -> Solucion
dpll (m,f) = if success(m,f) == True
	         then (m,f)
	         else if conflict (m,f) == True
	         	  then error "FAIL"
	         	  else dpllsearch (m,f)

main :: Solucion -> Solucion
main (m,f) = dpll(m,f) 

-- Ejemplos

bueno = [[Neg (V "P"), V "R", Neg (V "T")],[Neg (V "Q"), Neg (V "R")],[V "P",Neg (V "S")],[Neg (V "P"), V "Q", Neg (V "R"), Neg (V "S")]]
exe1 = [[V "p", V "q"],[Neg (V "q")],[Neg (V "p"), V "q", Neg (V "r")]]
exe2 = [[V "p", V "q"],[V "p", Neg (V "q")],[V "r", V "q"],[V "r", Neg (V "q")]]    
exe3 = [[V "p", Neg (V "q")],[Neg (V "p"), V "q"],[V "q", Neg (V "r")],[Neg (V "q"), Neg (V "r")]]
exe4 = [[V "p", V "q"], [V "r", Neg (V "q"), Neg (V "s")], [Neg (V "p"), V "s"], [Neg (V "r")]]
exe5 = [[V "p", V "q", V "r"], 
        [V "p", Neg (V "q"), Neg (V "r")],
        [V "p", Neg (V "w")],
        [Neg (V "q"), Neg (V "r"), Neg (V "w")],
        [Neg (V "p"), Neg (V "q"), V "r"],
        [V "u", Neg (V "x")],
        [V "u", V "x"],
        [V "q", Neg (V "u")],
        [Neg (V "r"), Neg (V "u")]]
exe6 = [[V "p"], [Neg (V "p")]]

ejemplo1 = main ([], exe1)
ejemplo2 = main ([], exe2)
ejemplo3 = main ([], exe3)
ejemplo4 = main ([], exe4)
ejemplo5 = main ([], exe5)   
ejemplo6 = main ([], bueno)   
ejemplo7 = main ([], exe6)