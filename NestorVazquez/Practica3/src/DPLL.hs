{-
- Logica Conmputacional 2020-2 
- Practica 3, Implementación del algoritmo dpll.
- Creación: Pedro Juan Salvador Sánchez Pérez
-}

module DPLL where

import LProp
import Data.List

type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

-- Seccion de funciones para la regla de la clausula unitaria

unit :: Solucion -> Solucion
unit ([],[]) = ([],[])
unit (m,f) = (agrega_unitaria_de_formula f, delete (agrega_unitaria_de_formula f) f)


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
elim s@(m,f) = error "Funcion a implementar"

-- Seccion de funciones para la regla de reduccion

red :: Solucion -> Solucion
red s@(m,f) = error "Funcion a implementar"

-- Seccion de funciones para la regla de separacion

split :: Solucion -> Solucion
split s@(m,f) = error "Funcion a implementar"

-- Seccion de funciones para la regla de conflicto

conflict :: Solucion -> Bool
conflict (m,f) = if f == [[]]
                 then True
                 else False

-- Seccion de funciones para la regla de exito

success :: Solucion -> Bool
success (m,f) = error "Funcion a implementar"

-- Ejemplos

exa0 = [[Neg (V "P"), V "R", Neg (V "T")],[Neg (V "Q"), Neg (V "R")],[V "P",Neg (V "S")],[Neg (V "P"), V "Q", Neg (V "R"), Neg (V "S")]]
exa1 = [[V "p", V "q"],[Neg (V "q")],[Neg (V "p"), V "q", Neg (V "r")]]
exa2 = [[V "p", V "q"],[V "p", Neg (V "q")],[V "r", V "q"],[V "r", Neg (V "q")]]    
exa3 = [[V "p", Neg (V "q")],[Neg (V "p"), V "q"],[V "q", Neg (V "r")],[Neg (V "q"), Neg (V "r")]]
exa4 = [[V "p", V "q"], [V "r", Neg (V "q"), Neg (V "s")], [Neg (V "p"), V "s"], [Neg (V "r")]]
exa5 = [[V "p", V "q", V "r"], 
        [V "p", Neg (V "q"), Neg (V "r")],
        [V "p", Neg (V "w")],
        [Neg (V "q"), Neg (V "r"), Neg (V "w")],
        [Neg (V "p"), Neg (V "q"), V "r"],
        [V "u", Neg (V "x")],
        [V "u", V "x"],
        [V "q", Neg (V "u")],
        [Neg (V "r"), Neg (V "u")]]
exa6 = [[V "p"], [Neg (V "p")]]     