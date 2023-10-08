module RSA where
import Tipos
import Aritmetica


{-(4) Escribir la función: claves :: Integer -> Integer -> (Integer, Integer, Integer) que dado dos números 
primos p y q, calcula los valores e,d y n utilizados por RSA para generar las claves y los devuelve en una 
tupla en ese orden.-}

{- La función "claves" depende de la función auxiliar "clavesDesde", que toma los primos "p" y "q" y el numero 
e". Si el inverso multiplicativo "d" existe (pusimos que sea mayor a 0 para asegurarnos de que exista, ya que 
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
 | otherwise = clavesDesde p q (m - 1)
  where m = (p - 1)*(q - 1)


{-(5) Escribir la función: codificador :: ClPub -> Mensaje -> Cifrado que dada la clave pública y una secuencia 
de caracteres, la codifica caracter a caracter y devuelve el mensaje encriptado.-}

{- la función "codificacion" depende principalmente de la función "codificacion" (que contiene las funciones
modExp" y "aEnteros", ambas dadas previamente); que toma la clave publica (e, n) y el mensaje codificado m.

La función funciona principalmente con las reglas del RSA: El mensaje se pasa a una lista con un numero entero 
por cada letra; entonces: Si mcd(m, n) = 1 devuelve m^e (mod n) del primer número y lo agrega a la recursión 
hecha con las demas "letras"; y si mcd(M, n) /= 1, el número de la primera letra pasará a ser negativo y se
agregará a la lista formada por la recursión hecha con las demás letras del mensaje. El caso base es cuando el 
mensaje está vacío (es una lista vacía) y devuelve una lista vacía.

Finalmente, "codificador" es la codificación con la clave pública (e, n) con la lista de enteros que forma el
mensaje.-}

codificacion :: (Integer, Integer) -> [Integer] -> [Integer]
codificacion (e, n) m | m == [] = []
 | mcd (head m) n == 1 = modExp (head m) e n : codificacion (e, n) (tail m)
 | otherwise = -(head m) : codificacion (e, n) (tail m)

codificador :: Clpub -> Mensaje -> Cifrado
codificador (e, n) m = codificacion (e,n) (aEnteros m)


{-(6) Escribir la función: decodificador :: ClPri -> Cifrado -> Mensaje que dada la clave privada y un mensaje 
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