type Complejo = (Float,Float)

--Problema 1
puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x,y) (w,z) = ((x+w)/2,(y+z)/2)

--Problema2
raices :: Float -> Float -> Float -> (Complejo,Complejo)
raices a b c= if r >=0
                then (((-b+sqrt(s))/q, 0),((-b-sqrt(s))/q, 0))
                else ((-(b)/q, sqrt(s)/q), (-b/q, -(sqrt(s))/q))
                where
                r= b^2-(4*a*c)
                s = -(r)
                q = 2*a

--Problema 3
segmento :: Int -> Int -> [a] -> [a]
