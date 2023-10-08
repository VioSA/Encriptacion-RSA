module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits


{-(1) Escribir la función: criba :: Integer -> Set Integer que, dado un número natural n, devuelve un conjunto 
de enteros con todos los primos positivos menores a n.-}

{- la función "criba" depende de la función "esPrimo" (y las funciones necesarias para crearla) creada en
clase. Entonces, si n - 1 (porque la consigna pide que sean los primos menores estrictos a n) es un primo, 
entonces se agrega al conjunto y se hace la recursión sobre n - 1; y si no es primo, solo se hace la 
recursión. Si n es menor o igual a 0 (el 1 está incluido como no primo en "esPrimo")  no devuelve nada 
a pedido de la consigna.-}

menorDivisorDesde:: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
 | otherwise = menorDivisorDesde n (k + 1)

menorDivisor:: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

criba :: Integer -> Set Integer
criba n | n <= 0 = []
 | esPrimo (n - 1) = (n - 1) : criba (n - 1)
 | otherwise = criba(n - 1)


{-(2) Escribir la función: coprimoCon:: Integer -> Integer que dado un número entero n > 2, encuentra otro 
número x, con 1 < x < n − 1, de tal forma que n y x sean coprimos (si es que existe). Notar que puede haber 
múltiples respuestas válidas, la función puede devolver cualquiera de ellas.-}

{- la función "coprimoCon" depende de la función auxiliar "coprimoConDesde", que pide varias cosas:
como "casos base" (para explicitar el rango posible de x a como lo pide la consigna), si x es mayor o igual a 
n - 1 o es menor o igual a 1, por definición en la consigna no existe numero que sea coprimo con n.
También pide que si x no divide a n y viceversa (por definición de coprimos), entonces son coprimos; y si esto
no pasa, hace recursión con x + 1.
Entonces, coprimoCon se define con coprimoConDesde desde x = 2 (por pedido de la consigna).-}

coprimoConDesde:: Integer -> Integer -> Integer
coprimoConDesde n x | x >= (n - 1) || x <= 1 = undefined
 | mod n x /= 0 && mod x n /= 0 = x
 | otherwise = coprimoConDesde n (x + 1)
 
coprimoCon:: Integer -> Integer
coprimoCon n = coprimoConDesde n 2


{-(3) Escribir la función: inversoMultiplicativo:: Integer -> Integer -> Integer que dados dos números n y m, 
calcula el inverso multiplicativo de n módulo m. Notar que esto sólo se puede hacer si los dos números son 
coprimos, y que se puede utilizar el algoritmo de Euclides extendido para calcularlo.-}

{- La función "inversoMultiplicativo" depende de las funciones "emcd" (que depende de "mcd", vista en clase), 
que devuelve la versión extendida del algoritmo de Euclides, y de la función "esCoprimos", que devuelve True si
al maximo comun divisor entre dos números es el 1 (por definición).

La función toma un número n y el módulo m y primero se asegura de que sean coprimos, porque si no no existe
inverso a la congruencia.

En caso de que sean coprimos, devuelve el número que multiplica a "a" en el algoritmo de Euclides Extendido
(Es decir, sa + tb = (a : b)), que es "s". como por definición dada en clase, s = t', debe devolverlo.

El problema es que t' puede ser negativo, lo cual aunque es válido en la congruencia es mas complicado de leer,
por lo que se le va sumando "m" las veces necesarias hasta que se vuelva un número positivo con la misma 
congruencia.-}
 
mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

esCoprimos:: Integer -> Integer -> Bool
esCoprimos n m = mcd n m == 1

emcd :: Integer -> Integer -> (Integer, Integer, Integer)
emcd n 0 = (abs n, signum n, 0)
emcd n m = (d, t', s' - (div n m) * t')
 where (d, s', t') = emcd m (mod n m)

{- Recuerdo: (a : b) = s*a + t*b, con s = t', t = s' − (a/b)t'-}

inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo n m | not (esCoprimos n m) = undefined 
 | t' <= 0 = t' + m
 | otherwise = t'
     where (d, s', t') = emcd m (mod n m)


-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
