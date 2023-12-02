# Tarea 5 - Laberintos

Resolver el laberinto provisto por el profesor utilizando tres algoritmos:

- UniformCost: solo contempla el _costo_, en este caso la longitud del camino
  como criterio de aptitud.
- Greedy: se contempla sólamente la _similitud_, en este caso con la evaluación
  de la heurística definida.
- A\*: contempla tanto el costo (la longitud del camino) y la similitud (la
  evaluación de la heurística definida).

Para este programa, se plantearon tres heurísticas que son intercambiables
mediante el uso de un predicado dinámico (`heurística/1`):

1. Distancia Manhattan:
   $$
   g(x) = |x_1 - x_2| + |y_1 - y_2|
   $$
2. Distancia Euclidiana:
   $$
   g(x) = \sqrt{(x_1 - x_2)^2 + (y_1 - y_2)^2}
   $$
3. Guíada: Esta heurística (propuesta por mí), evade el desperdicio de recursos
   computacionales mediante el uso de la persona ejecutando el programa
   como heurística en sí mismo.

## Pruebas

Programar los algoritmos no sirve de mucho si no podemos compararlos y entender
cuáles son las ventajas y desventajas de cada uno. Se programó un predicado
`prueba_general/0` el cual ejecuta los tres algoritmos, con cada una de las
heurísticas (distancia euclidiana y manhattan) y hace uso del predicado `time/1`
para mostrar los resultados.

Un ejemplo de resultados son:

```txt
Distancia euclidiana
A*:
% 11,343,723 inferences, 1.236 CPU in 1.248 seconds (99% CPU, 9175431 Lips)
Greedy:
% 4,909,923 inferences, 0.493 CPU in 0.496 seconds (99% CPU, 9960710 Lips)
UniformCost:
% 10,862,001 inferences, 1.217 CPU in 1.222 seconds (100% CPU, 8927926 Lips)
Distancia de manhattan
A*:
% 10,319,542 inferences, 1.088 CPU in 1.097 seconds (99% CPU, 9481065 Lips)
Greedy:
% 4,976,107 inferences, 0.488 CPU in 0.490 seconds (100% CPU, 10194120 Lips)
UniformCost:
% 10,862,001 inferences, 1.221 CPU in 1.228 seconds (99% CPU, 8897642 Lips)
Longitud de los caminos:
Algoritmo        | Euclidiana    | Manhattan
A*               | 234           | 234
Greedy           | 256           | 256
UniformCost      | 234           | 234
```

De los cuales podemos concluir que:

- En A\* es irrelevante el cambio entre la distancia Manhattan y la distancia
  Euclidiana como heurísticas en términos del camino encontrado, pero cambia la
  cantidad de _inferencias_, implicando un costo computacional mayor.
- En Greedy se puede apreciar el mismo patrón, aunque el camino encontrado
  por Greedy es más largo, pero su tamaño es el mismo usando cualquiera de las
  dos heurísticas.
- En UniformCost realmente no inciden las heurísticas en el algoritmo, por lo que
  los resultados son idénticos en ambos casos.

---

Humberto Alcocer, 2023.
