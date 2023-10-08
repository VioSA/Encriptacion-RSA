type Complejo = (Float, Float)


{- 1. re :: Complejo -> Float-}

re :: Complejo -> Float
re (a, _) = a

{- 2. im :: Complejo -> Float-}

im :: Complejo -> Float
im (_, b) = b

{- 3. conjugado :: Complejo -> Complejo-}

conjugado :: Complejo -> Complejo
conjugado (a, b) = (a, -b)

{- 4. suma :: Complejo -> Complejo -> Complejo-}

suma :: Complejo -> Complejo -> Complejo
suma (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

{- 5. producto :: Complejo -> Complejo -> Complejo-}

producto :: Complejo -> Complejo -> Complejo
producto (a, b) (c, d) = (a * c - b * d, a * d + b * c)

{- 6. inverso :: Complejo -> Complejo-}

--Obs: no se puede usar div porque son floats, no int.

inverso :: Complejo -> Complejo
inverso z |sumaCuadrados == 0 = undefined
 | otherwise = (a / sumaCuadrados, (-1) * (b / sumaCuadrados))
 where  a = re z
        b = im z
        sumaCuadrados = a**2 + b**2

{- 7. cociente :: Complejo -> Complejo -> Complejo-}
-- (a + bi) / (c + di) = (a + bi) * (c + di)**(-1)

cociente:: Complejo -> Complejo -> Complejo
cociente (a, b) (c, d) = producto (a, b) (inverso (c, d))

{- 8. potencia :: Complejo -> Int -> Complejo-}

potencia:: Complejo -> Int -> Complejo
potencia (a, b) 0 = (1, 0)
potencia (a, b) n | n < 0 = inverso (potencia (a, b) n)
 | otherwise = producto (a, b) (potencia (a, b) (n - 1))

{- 9. Dada una función cuadrática ax2 + bx + c con a, b, c en R, a /= 0, definir la función 
solucionesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)que tome los coeficientes a, b y c y 
devuelve las dos raíces. En caso de haber una sola, devolverla dos veces.-}

solucionesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
solucionesCuadratica a b c = ((terminoReal + resultadoReal, resultadoIm), 
 (terminoReal - resultadoReal, (-1) * resultadoIm))
 where discriminante = b**2 - 4 * a * c
       terminoReal = ((-1) * b) / (2 * a)
       divRaizDiscriminante = sqrt(abs(discriminante)) / (2 * a)
       resultadoIm | discriminante < 0 = divRaizDiscriminante
        | otherwise = 0
       resultadoReal | discriminante > 0 = divRaizDiscriminante
        | otherwise = 0

{- 10. modulo :: Complejo -> Float-}

modulo :: Complejo -> Float
modulo (a, b) = sqrt (a**2 + b**2)

{- 11. argumento :: Complejo -> Float-}

argumento :: Complejo -> Float
argumento (a, b) = atan (b / a)

{- 12. implementar la función pasarACartesianas :: Float -> Float -> Complejo que toma r ≥ 0 y θ en [0, 2π) y 
devuelve el complejo z que tiene módulo r y argumento θ.-}

pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r tita | r >= 0 && r < 2 * pi = (r * cos tita, r * sin tita)
 | r < 0 = (r * cos (tita + 2 * pi), r * sin (tita + 2 * pi))
 | otherwise = (r * cos (tita - 2 * pi), r * sin (tita - 2 * pi))

{- 13. raizCuadrada :: Complejo -> (Complejo,Complejo) que dado z en C, devuelve los dos complejos w que 
satisfacen w2 = z.-}



{- 14. SolucionesCuadraticaCoefComplejos :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo) que dado un 
polinomio az2 + bz + c, con a, b, c en C, a 6= 0, implementar la función que devuelve sus dos raices.-}