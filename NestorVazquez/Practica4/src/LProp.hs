{-
- Lógica computacional 2020-2
- Practica 3
- Alumno: José David Ramírez Rojas
- Número de cuenta: 316184924
- Correo: josedavidrr@ciencias.unam.mx
- Alumno: Néstor Semer Vázquez Cordero
- Número de cuenta: 316041625
- Correo: nestor2502@ciencias.unam.mx
-}

module LProp where

-- | VarP. Tipo que representa el conjunto de variables proposicionales.
type VarP = String

-- | Prop. Tipo que representa el conjunto de fórmulas de la lógica
-- proposicional.
data Prop = TTrue
          | FFalse
          | V VarP
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Imp Prop Prop
          | Equiv Prop Prop 

-- / se intancia la clase Show para poder especificar como queremos que se impriman la logica proposicional 
-- / TTrue en terminal se vera como "T", Neg (V "p") se vera como ~(p)
instance Show Prop where
    show TTrue = "T"
    show FFalse = "F"
    show (V x) = show x
    show (Neg p) = "~("++ show p ++")"
    show (Conj p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
    show (Disy p q) = "(" ++ show p ++ " v " ++ show q ++")"
    show (Imp p q) = "(" ++ show p ++ " -> " ++ show q ++")"
    show (Equiv p q) = "(" ++  show p ++ " <-> " ++ show q ++")"

-- / se insatancia la clase Eq para poder comparar formulas proposicionales
-- TTrue == False = False
instance Eq Prop where
    (==) TTrue TTrue = True
    (==) FFalse FFalse = True
    (==) (V x) (V y) = x == y
    (==) (Neg p) (Neg q) = (==) p q
    (==) (Conj p q) (Conj r s) = (==) p r && (==) q s
    (==) (Disy p q) (Disy r s) = (==) p r && (==) q s
    (==) (Imp p q) (Imp r s) = (==) p r && (==) q s
    (==) (Equiv p q) (Equiv r s) = ((==) p r && (==) q s) || ((==) p s && (==) q r)
    (==) p q = False

    
-- / se instancia la clase Ord para poder dar orden a las formulas proposicionales 
-- ~p > p = False
instance Ord Prop where
    (<) p q = peso p < peso q
    (>) p q = peso p > peso q 
    (<=) p q = peso p <= peso q 
    (>=) p q = peso p >= peso q 
    min p q = if peso p <= peso q then p else q
    max p q = if peso p >= peso q then p else q

-- | peso. Función que dada una fórmula devuelve el número de sus conectivos.
--
-- --> peso (Conj (V 1) (Disy (V 2) (FFalse))) = 2
-- --> peso (Conj (V 1) (Disy (V 2) (Neg (V 3)))) = 3
peso :: Prop -> Int
peso (Neg p) = 1 + peso p
peso (Conj p q) = 1 + peso p + peso q
peso (Disy p q) = 1 + peso p + peso q
peso (Imp p q) = 1 + peso p + peso q
peso (Equiv p q) = 1 + peso p + peso q
peso _ = 0 

-- --> elimEquiv (Equiv (V p) (V q)) = (Conj (Imp (V p) (V q)) (Imp (V q) (V p)))
elimEquiv :: Prop -> Prop
elimEquiv phi = case phi of 
  TTrue -> TTrue
  FFalse -> FFalse
  V beta -> V beta
  Neg beta -> Neg (elimEquiv beta)
  Conj beta gamma -> Conj (elimEquiv beta )(elimEquiv gamma )
  Disy beta gamma -> Disy (elimEquiv beta ) (elimEquiv gamma )
  Imp beta gamma -> Imp(elimEquiv beta ) (elimEquiv gamma)
  Equiv beta gamma -> Conj(Imp (elimEquiv beta) (elimEquiv gamma)) (Imp (elimEquiv gamma) (elimEquiv beta))

-- | elimImp. Funció que dada una fórmula devuelve su equivalente que no contiene implicaciones.
--
-- --> elimEquiv (Imp (V p) (Disy (V q) (FFalse))) = Disy (Neg (V p)) (Disy (V q) FFalse)
elimImp :: Prop -> Prop
elimImp phi = case phi of 
  TTrue -> TTrue
  FFalse -> FFalse
  V beta -> V beta
  Neg beta -> Neg (elimImp beta)
  Conj beta gamma -> Conj (elimImp beta )(elimImp gamma )
  Disy beta gamma -> Disy (elimImp beta ) (elimImp gamma )
  Imp beta gamma -> Disy (Neg beta ) (gamma)
  Equiv beta gamma -> Equiv(elimImp beta) (elimImp gamma)


-- | elimIE. Función que dada una fórmula devuelve su equivalente que no contiene implicaciones,
-- ni equivalencias.
--
elimIE :: Prop -> Prop
elimIE p = elimImp $ elimEquiv p

-- / funcion que recibe una formula de la logica proposicional y que devuelve otra formula de la logica proposicional que es logicamente
-- equivalente pero las negaciones que existen solo aplican a formulas atomicas. tambien elimina la doble negacion. la funcion supone
-- que la fomula ya no tiene implicaciones ni equivalencias.
meteNeg :: Prop -> Prop
meteNeg p = case p of 
  TTrue -> TTrue
  FFalse -> FFalse
  V beta -> V beta
  Neg beta -> auxMeteNeg (beta)
  Conj beta gamma -> Conj (meteNeg beta )(meteNeg gamma )
  Disy beta gamma -> Disy (meteNeg beta ) (meteNeg gamma )

auxMeteNeg :: Prop -> Prop
auxMeteNeg p = case p of 
  TTrue -> TTrue
  FFalse -> FFalse
  V beta -> Neg (V beta)
  Neg beta -> beta
  Conj beta gamma -> Disy (Neg (beta) )(Neg (gamma) )
  Disy beta gamma -> Conj (Neg (beta) )(Neg (gamma) )

  
-- / funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente tal que este en forma normal negativa,
fnn :: Prop -> Prop 
fnn p = meteNeg (elimIE p)

-- / funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente pero que distribuye la disyuncion sobre la conjuncion (Ej. p v (q ^ r) = (p v q) ^ (p v r)). la funcion supone que la formula 
-- esta en forma normal negativa
dist :: Prop -> Prop
dist p = case p of 
  TTrue -> TTrue
  FFalse -> FFalse
  V beta -> V beta
  Neg beta -> Neg (dist(beta))
  Conj beta gamma -> Conj (dist beta)(dist gamma)
  Disy beta gamma -> if esConjuncion(gamma)
                     then Conj (Disy (beta)(auxDist1 (gamma)))(Disy (beta)(auxDist2 (gamma))) 
                     else Disy (dist beta)(dist gamma)

esConjuncion :: Prop -> Bool
esConjuncion (Conj _ _) = True
esConjuncion _ = False

--auxiliar que obtiene la primera parte de una conjuncion   
auxDist1 :: Prop -> Prop
auxDist1 p = case p of 
  TTrue -> TTrue
  FFalse -> FFalse
  V beta -> V beta
  Neg beta -> Neg beta
  Conj beta gamma -> beta
  Disy beta gamma -> Disy (beta)(gamma) 

--auxiliar que obtiene la segunda parte de una conjuncion
auxDist2 :: Prop -> Prop
auxDist2 p = case p of 
  TTrue -> TTrue
  FFalse -> FFalse
  V beta -> V beta
  Neg beta -> Neg beta
  Conj beta gamma -> gamma
  Disy beta gamma -> Disy (beta)(gamma) 

-- / funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente tal que este en forma normal conjuntiva,
cnf :: Prop -> Prop
cnf p = dist $ fnn p 