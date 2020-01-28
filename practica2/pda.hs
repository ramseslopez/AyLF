module PDA where

        import Stack

        -- Tipo de dato algebraico para simular los estados de un autómata
        data State = Q Int deriving (Show, Eq)

        -- Sinónimos para símbolos, alfabeto y la función de transición
        type Symbol = String
        type Alphabet = [String]
        type Delta = State -> Symbol -> Stack -> (State, Stack)

        -- Tipo de dato algebraico Automata, implementación de un autómata finito determinista
        data Automata = PDA {q::[State], s::Alphabet, d::Delta, q0::State, f::[State]}
        -- q : Conjunto de Estados
        -- s : Alfabeto de entrada
        -- d : Función de transición
        -- q0 : Estado inicial
        -- f : Conjunto de estados finales

        -- Sinónimo de un autómata de pila
        type Machine = (Automata, Stack)

        -- Sinónimo de una configuración
        type Config = (State, String, Stack)

        -- Función que recibe una máquina, una cadena e imprime el procesamiento formal de
        -- la cadena con configuraciones
        compute :: Machine -> String -> [[Config]]
        --compute (pda, p) "" = if p == [] then procesa
        --compute (pda, p) (x:xs) = [[((q0 pda), (x:xs), p)]] ++ compute (pda, p) xs
        compute a b = error ""

        -- función de transición extendida, es necesario cargar con la función de transición >:(
        --deltaStar :: State -> String -> Delta -> State
        --deltaStar q "" _ = q
        --deltaStar q (a:w) d = deltaStar (d q a) w d

        -- función que decide si una cadena es aceptada o no por un autómata.
        --accept :: Automata -> String -> Bool
        --accept fda w = deltaStar (q0 fda) w (d fda) `elem` (f fda)

        -- Definición de un autómata que acepta el lenguaje L = {a^n b^m c^k | n = m o m = k}
        -- definimos la función de transición
        delta1 :: Delta
        delta1 (Q 0) "a" p = ((Q 1), apilar "A" p)
        delta1 (Q 0) "" p = ((Q 4), p)
        delta1 (Q 1) "a" p = ((Q 1), apilar "A" p)
        delta1 (Q 1) "b" p = ((Q 2), desapilar p)
        delta1 (Q 2) "b" p = ((Q 2), desapilar p)
        delta1 (Q 2) "c" p = ((Q 3), p)
        delta1 (Q 2) "" p = ((Q 3), p)
        delta1 (Q 3) "c" p = ((Q 3), p)
        delta1 (Q 4) "a" p = ((Q 4), p)
        delta1 (Q 4) "b" p = ((Q 5), apilar "A" p)
        delta1 (Q 5) "b" p = ((Q 5), apilar "A" p)
        delta1 (Q 5) "c" p = ((Q 6), desapilar p)
        delta1 (Q 6) "c" p = ((Q 6), desapilar p)
        delta1 (Q 6) "" p = ((Q 7), desapilar p)

        -- definimos el autómata
        pda1 = PDA {
                q = [(Q 0), (Q 1), (Q 2), (Q 3), (Q 4), (Q 5), (Q 6), (Q 7)],
                s = ["a", "b", "c"],
                d = delta1,
                q0 = (Q 0),
                f = [(Q 3), (Q 7)]
        }
