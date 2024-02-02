# Proyecto Final - Agente Jugador de Mancala

Este proyecto consiste en la implementación de un Agente Jugador de Mancala en el lenguaje de programación Prolog. Mancala es un juego de estrategia de tablero para dos jugadores que involucra la distribución de piedras en hoyos. El objetivo del juego es recolectar la mayor cantidad de piedras en tu Mancala (un hoyo grande en tu lado del tablero) al finalizar el juego.

## Reglas del Juego

- El tablero de Mancala consta de dos filas de hoyos, cada una con 6 hoyos, y un Mancala para cada jugador.
- Al comienzo, se colocan una cierta cantidad de piedras en cada hoyo (usualmente 4).
- Los jugadores se turnan para tomar piedras de un hoyo en su fila y distribuirlas en los hoyos en sentido contrario a las agujas del reloj.
- Si la última piedra cae en el Mancala del jugador, el jugador obtiene otro turno.
- Si la última piedra cae en un hoyo vacío en su propia fila, el jugador captura todas las piedras en el hoyo opuesto y las coloca en su Mancala.
- El juego termina cuando todos los hoyos de una fila están vacíos para un jugador. El jugador que tenga más piedras en su Mancala gana.

## Horizonte de Búsqueda

El "horizonte de búsqueda" se refiere a la profundidad máxima hasta la cual el agente explorará posibles movimientos en el árbol de búsqueda del juego. Un horizonte de búsqueda más profundo permite al agente evaluar movimientos futuros y tomar decisiones más informadas, pero también puede aumentar significativamente el tiempo de cálculo.

## Ejemplos de Uso

Aquí se muestra cómo puedes usar el agente jugador de Mancala en Prolog:

```prolog
?- inicia_juego(Horizonte).
```

El `Horizonte` indicará la profunidad con la que las podas se realizan en el árbol
de búsqueda.

---

Humberto Alejandro Ortega Alcocer, 2024.
