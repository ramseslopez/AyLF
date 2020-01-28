module Stack where
 
    -- Pilas con listas
    type Stack = [String]
    
    -- pilaVacia es la pila vacía.
    pilaVacia :: Stack
    pilaVacia = []
    
    -- (esPilaVacia p) se verifica si p es la pila vacía
    esPilaVacia :: Stack -> Bool
    esPilaVacia [] = True
    esPilaVacia _ = False
    
    -- (apilar x p) es la pila obtenida añadiéndole x encima de la pila
    apilar :: String -> Stack -> Stack
    apilar x xs = (x:xs)
    
    -- (desapilar p) es la pila obtenida suprimiendo la cima de la pila p
    desapilar :: Stack -> Stack
    desapilar [] = error "desapilar de la pila vacía"
    desapilar (_:xs) = xs
    
    -- (cima p) es la cima de la pila p.
    cima :: Stack -> String
    cima [] = error "cima de la pila vacía"
    cima (x:_) = x