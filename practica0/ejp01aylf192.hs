-- Autómatas y lenguajes formales 2019-2
-- Ejercicio práctico 1


-- Función que regresa la lista de números de fibonacci
sFibonacci :: Int -> [Int]
sFibonacci n = [fibonacci n | n <- [1..]]


-- Función que elimina el elemento indicado en la lista
quitaElementos :: (Eq a) => [a] -> a -> [a]
quitaElementos [] a     = error "La lista no tiene elementos"
quitaElementos (x:xs) a = eliminaR a (x:xs)


-- Función que devuelve la lista de divisores propios de un número
divisoresPropios :: Int -> [Int]
divisoresPropios 0 = error "Error"
divisoresPropios n = [ x | x <- [1..n], mod n x == 0, x < n]


-- Función que nos indica si un número es perfecto
esPerfecto :: Int -> Bool
esPerfecto n
            | n == sumaDP n = True
            | otherwise     = False


-- Función que nos indica si dos números son amigos
sonAmigos :: Int -> Int -> Bool
sonAmigos n m
              | sumaDP n == m && sumaDP m == n = True
              | otherwise                      = False


-- Función que devuelve la suma de los dígitos de un número hasta la mínima expresión
supersuma :: Int -> Int
supersuma 0 = 0
supersuma n
          | mod (sp) 10 == 0 = supersuma(sp)
          | otherwise = (div spp 10) + (mod spp 10)
          where sp = (mod n 10) + supersuma(div n 10)
                spp = (mod n 10) + supersuma(div n 10)


-- Función que devuelve la reversa de una lista usando foldr
reversar :: [a] -> [a]
reversar = foldr rev []
                  where rev = (\x xs -> xs ++ [x])


-- Función que devuelve la reversa de una lista usando foldl
reversal :: [a] -> [a]
reversal = foldl rev2 []
                  where rev2 = (\x xs -> xs:x)


-- Funciones auxiliares

-- Función que calcula los números de fibonacci
fibonacci :: Int -> Int
fibonacci 0 = error "Te equivocaste"
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)


-- Función que calcula la suma de los divisores propios de un número
sumaDP :: Int -> Int
sumaDP 0 = 0
sumaDP n = sum (divisoresPropios n)


-- Función que se encarga de eliminar un elemento de la lista
eliminaR :: (Eq a) => a -> [a] -> [a]
eliminaR c [] = []
eliminaR c (x:xs)
              | c == x    = eliminaR c xs
              | otherwise = [x] ++ eliminaR c xs
