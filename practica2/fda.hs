module FDA where 

    -- Tipo de dato algebraico para simular los estados de un autómata
    data State = Q Int deriving (Show, Eq)
   
    -- Sinónimos para símbolos, alfabeto y la función de transición
    type Symbol = Char
    type Alphabet = [Char] 
    type Delta = State -> Symbol -> State 
   
    -- Tipo de dato algebraico Automata, implementación de un autómata finito determinista 
    data Automata = FDA {q::[State], s::Alphabet, d::Delta, q0::State, f::[State]}
    -- q : Conjunto de Estados
    -- z : Alfabeto de entrada
    -- d : Función de transición
    -- q0 : Estado inicial
    -- f : Conjunto de estados finales
   
    -- función de transición extendida, es necesario cargar con la función de transición >:(
    deltaStar :: State -> String -> Delta -> State
    deltaStar q "" _ = q
    deltaStar q (a:w) d = deltaStar (d q a) w d
   
    -- función que decide si una cadena es aceptada o no por un autómata. 
    accept :: Automata -> String -> Bool
    accept fda w = deltaStar (q0 fda) w (d fda) `elem` (f fda)
   
    -- Definición de un autómata que acepta el lenguaje L = {x en {a,b}* | x termina en aa}
   
    -- definimos la función de transición 
   
    delta1 :: Delta
    delta1 (Q 0) 'a' = (Q 1)
    delta1 (Q 0) 'b' = (Q 0)
    delta1 (Q 1) 'a' = (Q 2)
    delta1 (Q 1) 'b' = (Q 0)
    delta1 (Q 2) 'a' = (Q 2)
    delta1 (Q 2) 'b' = (Q 0)
   
    -- definimos el autómata
    fda1 = FDA {
            q = [(Q 0), (Q 1), (Q 2)],
            s = ['a', 'b'],
            d = delta1,
            q0 = (Q 0),
            f = [(Q 2)]
   }
   
   -- Algunos ejemplos 
   
   -- accept fda1 "ababaa" se espera que regrese True
   -- accept fda1 "ababaab" se espera que regrese False