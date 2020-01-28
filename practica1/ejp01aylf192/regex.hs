module Regex where

 import Data.List

 -- Tipo de dato algebraico para representar Expresiones Regulares
 data Regex = Void
            | Epsilon
            | Symbol Char           -- El símbolo representado por el caracter que recibe
            | Star Regex            -- r*
            | Concat Regex Regex    -- (rs)
            | Add Regex Regex       -- (r + s)
            deriving (Eq)

 -- Sinónimo para representar lenguajes como listas de cadenas.
 type Language = [String]

 -- Instancia de Show del tipo Regex, para que se impriman con formato en la consola.
 instance Show Regex where
   show Void = "ø"
   show Epsilon = "ε"
   show (Symbol c) = c:[]
   show (Star r) = show r ++ "*"
   show (Concat r s) = "(" ++ show r ++ show s ++ ")"
   show (Add r s) = "(" ++ show r ++ " + " ++ show s ++ ")"

  ------------------- DENOTACIÓN -----------------------

 -- EJERCICIO 1
 simpl :: Regex -> Regex
 simpl Void                     = Void
 simpl Epsilon                  = Epsilon
 simpl (Star Void)              = Epsilon
 simpl (Star Epsilon)           = Epsilon
 simpl (Star (Add Void a))      = (Star (simpl a))
 simpl (Star (Add a Void))      = (Star (simpl a))
 simpl (Add Void Void)          = Void
 simpl (Add Void a)             = a
 simpl (Add a Void )            = a
 simpl (Add Epsilon Void)       = Epsilon
 simpl (Add Void Epsilon)       = Epsilon
 simpl (Concat Epsilon Epsilon) = Epsilon
 simpl (Concat Void Void)       = Void
 simpl (Concat Void _)          = Void
 simpl (Concat _ Void)          = Void
 simpl (Concat Epsilon a)       = a
 simpl (Concat a Epsilon)       = a
 simpl (Add (Star a) Void)      = (Star a)
 simpl (Add Void (Star a))      = (Star a)
 simpl (Concat (Star a) Void)   = Void
 simpl (Concat Void (Star a))   = Void
 simpl (Add a b)                = (Add (simpl a) (simpl b))
 simpl (Concat a b)             = (Concat (simpl a) (simpl b))
 simpl (Star (Add a Epsilon))   = Add (Star a) Epsilon
 simpl (Star (Add Epsilon a))   = Add Epsilon (Star a)
 simpl (Star (Add a Void))      = (Star a)
 simpl (Star (Add Void a))      = (Star a)
 simpl (Star a)                 = (Star (simpl a))
 simpl p                        = axSalomaa p

 -- EJERCICIO 2
 denot :: Regex -> Language
 denot Void         = []
 denot Epsilon      = [""]
 denot (Symbol a)   = [(chrToStr a)]
 denot (Add s t)    = (denot s) ++ (denot t)
 denot (Concat s t)
                  | t == Epsilon  = denot s
                  | s == Epsilon  = denot t
                  | s == Void     = []
                  | t == Void     = []
                  | otherwise     = error "error"

 -- EJERCICIO 3
 matchD :: String -> Regex -> Bool
 matchD a b
          | buscaN a (denot b) = True
          | otherwise          = False

 ----------------- --- DERIVADA ----------------------

 -- EJERCICIO 1
 deriv :: String -> Regex -> Regex
 deriv "" b     = b
 deriv (x:xs) p = deriv xs (derivSymbol (Symbol x) p)


 -- EJERCICIO 2
 matchV :: String -> Regex -> Bool
 matchV (x:xs) p
                | (deriv (x:xs) p) /= Void  = True
                | otherwise                 = False


----------------- --- Funciones auxiliares ----------------------

-- Función que transforma un caracter a un String
 chrToStr :: Char -> String
 chrToStr a = [a]

 -- Función que utiliza los axiomas de Salomaa para simplificar expresiones
 axSalomaa :: Regex -> Regex
 axSalomaa (Add t s)
                  | t == s = t
                  | t == Void = s
                  | s == Void = t
                  | t /= s = (Add t s)
                  | t == (Add Epsilon (Concat t (Star t))) && s == Epsilon = (Star t)
                  | t == (Add Epsilon (Concat s (Star s))) && t == Epsilon = (Star s)
                  | otherwise = error "error"
 axSalomaa (Concat t s)
                  | t == s                  = (Concat t s)
                  | t == Epsilon            = s
                  | s == Epsilon            = t
                  | t == Void || s == Void  = Void
                  | s == (Star t)           = (Star t)
                  | t == (Star s)           = (Star s)
                  | otherwise               = error "error"


 -- Funcion que sirve para calcular la deriva a partir de un símbolo
 derivSymbol :: Regex -> Regex -> Regex
 derivSymbol _ Void                  = Void
 derivSymbol _ Epsilon               = Void
 derivSymbol (Symbol a) (Symbol b)
                                | a == b    = Epsilon
                                | otherwise = Void
 derivSymbol (Symbol a) (Add r s)    = simpl (Add (derivSymbol (Symbol a) r) (derivSymbol (Symbol a) s))
 derivSymbol (Symbol a) (Concat r s) = simpl (Add (Concat (derivSymbol (Symbol a) r) s) (Concat (nulD r) (derivSymbol (Symbol a) s)))
 derivSymbol (Symbol a) (Star r)     = simpl (Concat (derivSymbol (Symbol a) r) (Star r))

 -- Funcion que calcula la nulidad de una expresion regular
 nulD :: Regex -> Regex
 nulD Void              = Void
 nulD Epsilon           = Epsilon
 nulD (Symbol a)        = Void
 nulD (Star (Symbol a)) = Epsilon
 nulD (Add r s)         = simpl (Add (nulD r) (nulD s))
 nulD (Concat r s)      = simpl (Concat (nulD r) (nulD s))

-- Funcion que se encarga de buscar un elemento en el lenguaje
 buscaN :: String -> Language -> Bool
 buscaN a [] = False
 buscaN a (b:cs)
                | a == b    = True
                | otherwise = buscaN a cs
