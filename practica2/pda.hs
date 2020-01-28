module PDA where

  -- Tipo de dato algebraico para simular los estados de un autómata
  data State = Q Int deriving (Show, Eq)

  -- data Symbol = C Char | epsilon

  -- Tipo de dato algebraico para simular el fondo de la pila
  data StackBottom = Z deriving (Show, Eq)

  -- Sinónimos para símbolos, alfabeto y la función de transición
  type Symbol = Char
  type Alphabet = [Symbol]
  type StackAlphabet = [Symbol]
  type Delta = State -> Symbol -> Symbol -> [(State, [Symbol])]

  -- Tipo de dato algebraico Automata, implementación de un autómata de pila
  data Automata = PDA {q::[State], z::Alphabet, t::StackAlphabet, d::Delta, q0::State, z0::StackBottom, f::[State]}
  -- q : Conjunto de Estados
  -- z : Alfabeto de entrada
  -- t : alfabeto de la pila
  -- d : Función de transición
  -- q0 : Estado inicial
  -- z0 : fondo de la pila
  -- f : Conjunto de estados finales

data Stack =
