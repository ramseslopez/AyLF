López Soto Ramses Antonio 31531997-4
Quintero Villeda Erik 31519934-5

La práctica fue muy conflictiva y aún en algunos casos lo es.
La implemntación de los conceptos fue difícil, pues nos costó mucho trabajo
abstraer todo a código de haskell pero pudimos lograr programarlo, tal vez de
una forma muy poco eficiente, pero funciona.

Para que algunas funciones funcionaran de una mejor forma, fue necesario
implementar funciones auxiliares para brindar ayuda a a fórmula en sí, como :

-- axSalomaa: fue necesario para que simplificara las expresiones regulares
              utilizando los axiomas de Salomaa, y así, simpl pudiera simplificar
              más fácil.

-- chrToStr:  fue necesaria para la función denot para que transformara
              símbolos a cadenas y así fuera un caso base.

-- derivSymbol: fue necesaria de implementar para que derivara son un solo
                símbolo a una espresion regular y así poder facilitar el
                cálculo de a deriva con una cadena en la funci+on deriv.

-- nulD:        fue necesaria para el cálculo de la derivada con un símbolo,
                la cual se usó en deriv.

-- buscaN:      fue necesaria para la funcion de matchD, la cual se encarga de
                buscar a un símbolo/caracter dentro del lenguaje, que junto con
                denot nos dicen si un elemento está o no dentro.
