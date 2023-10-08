type Set a = [a]
type Clpub = (Integer, Integer) -- Clave (e, n)
type Clpri = (Integer, Integer) -- Clave (d, n)
type Cifrado = [Integer]
type Mensaje = [Char]

{- 1. Escribir la función: criba :: Integer -> Set Integer que, dado un número natural n, devuelve un conjunto 
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
criba x | x <= 0 = []
 | esPrimo (x-1) = (x-1) : criba (x-1)
 | otherwise = criba(x-1)


{- 2. Escribir la función: coprimoCon:: Integer -> Integer Dado un número entero n > 2, encuentra otro número x, 
con 1 < x < n − 1, de tal forma que n y x sean coprimos (si es que existe). Notar que puede haber múltiples
respuestas válidas, la función puede devolver cualquiera de ellas.-}

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


{- 3. Escribir la función: inversoMultiplicativo:: Integer -> Integer -> Integer que dados dos números n y m, 
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


{- 4. Escribir la función: claves :: Integer -> Integer -> (Integer, Integer, Integer) Que dado dos números 
primos p y q, calcula los valores e, d y n utilizados por RSA para generar las claves y los devuelve en una 
tupla en ese orden.-}

{- La función "claves" depende de la función auxiliar "clavesDesde", que toma los primos "p" y "q" y el numero 
"e". Si el inverso multiplicativo "d" existe (pusimos que sea mayor a 0 para asegurarnos de que exista, ya que 
en la función "inversoMultiplicativo" nos aseguramos de que siempre sea positiva); entonces si hay un inverso 
multiplicativo, develve (e, d, n). Si no hay inverso, hace recursión con e - 1. El caso base es cuando e <= 0 
(por definición), lo que significa que no hay inverso multiplicativo, por lo que no hay solución.

Finalmente, "claves" usa la función "clavesDesde", que asegura que p y q sean primos y que e sea menor a 
m = (p - 1)*(q - 1) (me aseguro que e es mayor o igual a 0 en "clavesDesde").-}

clavesDesde:: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
clavesDesde p q e | e <= 0 = undefined
 | (inversoMultiplicativo e m) > 0 = (e, (inversoMultiplicativo e m), p*q)
 | otherwise = clavesDesde p q (e - 1)
  where m = ((p - 1)*(q - 1))

claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q | not (esPrimo p) || not (esPrimo q) = undefined
 | otherwise = clavesDesde p q (((p - 1)*(q - 1)) - 1) --cambiar a m-2.
 

{- 5. Escribir la función: codificador :: ClPub -> Mensaje -> Cifrado que dada la clave pública y una secuencia 
de caracteres, la codifica caracter a caracter y devuelve el mensaje encriptado.-}

--Recuerdo: clave pública a la dupla (e, n) y clave privada a la dupla (d, n).

{- la función "codificacion" depende principalmente de la función "codificacion" (que contiene las funciones
modExp" y "aEnteros", ambas dadas previamente); que toma la clave publica (e, n) y el mensaje codificado m.

La función funciona principalmente con las reglas del RSA: El mensaje se pasa a una lista con un numero entero 
por cada letra; entonces: Si mcd(m, n) = 1 devuelve m^e (mod n) del primer número y lo agrega a la recursión 
hecha con las demas "letras"; y si mcd(M, n) /= 1, el número de la primera letra pasará a ser negativo y se
agregará a la lista formada por la recursión hecha con las demás letras del mensaje. El caso base es cuando el 
mensaje está vacío (es una lista vacía) y devuelve una lista vacía.

Finalmente, "codificador" es la codificación con la clave pública (e, n) con la lista de enteros que forma el
mensaje.-}

aEnteros :: [Char] -> [Integer]
aEnteros = map (toInteger . ord)

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1

codificacion :: (Integer, Integer) -> [Integer] -> [Integer]
codificacion (e, n) m | m == [] = []
 | mcd (head m) n == 1 = modExp (head m) e n : codificacion (e, n) (tail m)
 | otherwise = -(head m) : codificacion (e, n) (tail m)
	
codificador :: Clpub -> Mensaje -> Cifrado
codificador (e, n) m = codificacion (e,n) (aEnteros m)


{- 6. Escribir la función: decodificador :: ClPri -> Cifrado -> Mensaje que dada la clave privada y un mensaje 
encriptado, la decodifica y devuelve la secuencia original.-}

{- La función "decoficador" toma a la dupla (d, n) como la clave privada y la lista c como el mensaje cifrado.
Si el primer numero de la lista (la primera letra) es mayor a 0, entonces hago ese numero elevado a la d modulo
n, lo paso a caracteres y lo concateno a la recursión hecha con el resto de los elementos de la lista; y si
no es positivo simplemente le cambio de signo, lo paso a caracteres y lo concateno a la recursión hecha con
el resto de la lista. El caso base es cuando la lista está vacía, por lo que devueve "".-}

decodificador :: Clpri -> Cifrado -> Mensaje
decodificador (d, n) c | c == [] = ""
 | head c >= 0 = aChars ([modExp (head c) d n]) ++ decodificador (d, n) (tail c)
 | otherwise = aChars [-(head c)] ++ decodificador (d, n) (tail c)