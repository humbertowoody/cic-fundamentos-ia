% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Tarea 5 - Laberintos
%           Implementación de A*, Greedy y UniformCost para la búsqueda de
%           caminos en laberintos. La heurística puede alternarse entre
%           distancia euclidiana y distancia de manhattan.
%
% Configuración del programa: En este programa utilicé fuertemente el concepto
%                             de predicados dinámicos, con ellos se define el
%                             estado meta y el estado inicial, además de
%                             definir la heurística que se utilizará.
%
% Predicados dinámicos:
%     - estado_meta/1     - Define el estado meta.
%       retractall(estado_meta(_)),assert(estado_meta([F, C]).
%     - estado_inicial/1  - Define el estado inicial.
%       retractall(estado_meta(_)),assert(estado_inicial([F, C]).
%     - heurística/2      - Define la heurística a utilizar.
%       heurística(X).                                              - Consultar la heurística definida actualmente.
%       retractall(estado_meta(_)), assert(heurística(euclidiana)). - Distancia euclidiana.
%       retractall(estado_meta(_)), assert(heurística(manhattan)).  - Distancia de manhattan.
%       retractall(estado_meta(_)), assert(heurística(guiada)).     - Heurística guiada por el usuario (¡Extra!)
%
% Propuesta de Ejecución: Todos los predicados no tienen argumentos de entrada,
%                         pero todos muestran la solución de la búsqueda de
%                         camino en el laberinto.
%     - A*
%       prueba_a_star.
%     - Greedy
%       prueba_greedy.
%     - UniformCost
%       prueba_uniform_cost.
%
% Heurística adicional: Guíada por el usuario.
%    - Esta heurística permite al usuario definir la heurística a utilizar
%      en tiempo de ejecución. Para ello, se debe definir el predicado
%      heurística/2 con el átomo 'guiada'.
%      assert(heurística(guiada)).
%    - En cada evaluación de aptitud, se le preguntará al usuario la
%      similitud entre el estado actual y el estado meta. La similitud
%      debe ser un número entero entre 0 y 100.
%   - Se le mostrará al usuario el estado actual y el estado meta, así como
%     las distancias euclidiana y de manhattan entre ambos estados para que
%     pueda definir la similitud con mejor criterio.
%    - La similitud se utiliza para ordenar la frontera, de tal forma que
%      los estados más similares al estado meta se evalúan primero.
%
%
% Fundamentos de Inteligencia Artificial, CIC, IPN, 2023.
% =============================================================================

% Consultamos el archivo que contiene el laberinto, el estado inicial y
% el estado final.
:- consult('laberinto-01.prolog').

% heurística/1
% heurística(-Heurística)
% Es cierto cuando Heurística unifica con la heurística a utilizar.
% Las heurísticas disponibles son:
%  - euclidiana: Distancia euclidiana entre el estado actual y el estado meta.
%  - manhattan: Distancia de manhattan entre el estado actual y el estado meta.
%  - guiada: Heurística guiada por el usuario.
:- dynamic heurística/1.
heurística(manhattan). % Heurística por defecto.

% expandir_nodo/2
% expandir_nodo(+Nodo, -ListaSucesores)
% Es cierto cuando ListaSucesores unifica con una lista
% que contiene a los sucesores de Nodo según el
% predicado acceso/2.
expandir_nodo(Nodo, ListaSucesores):-
  findall(Sucesor,
         (acceso(Nodo, SucesoresDirectos), member(Sucesor, SucesoresDirectos);
          acceso(Sucesor, SucesoresInversos), member(Nodo, SucesoresInversos)
         ),
          ListaTodosSucesores),
  list_to_set(ListaTodosSucesores, ListaSucesores). % Elimina duplicados.


% calcular_heurística/2
% calcular_heurística(+Estado, -Similitud)
% Es cierto cuando Similitud unifica con la similitud entre el estado
% actual y el estado meta. La similitud se calcula con la heurística
% definida en el predicado dinámico heurística/2.
calcular_heurística(Estado, Similitud):-
  heurística(guiada),
  estado_meta(EstadoMeta),
  format('Estado actual: ~w~n', [Estado]),
  format('Estado meta: ~w~n', [EstadoMeta]),
  distancia_euclidiana(Estado, EstadoMeta, Euclidiana),
  distancia_manhattan(Estado, EstadoMeta, Manhattan),
  format('Distancia euclidiana: ~w~n', [Euclidiana]),
  format('Distancia de manhattan: ~w~n', [Manhattan]),
  format('¿Cuál es la similitud (1-100) entre el estado actual y el estado meta?~n'),
  read(Similitud),
  Similitud >= 0,
  Similitud =< 100.
calcular_heurística(Estado, Similitud):-
  heurística(euclidiana),
  estado_meta(EstadoMeta),
  distancia_euclidiana(Estado, EstadoMeta, Similitud).
calcular_heurística(Estado, Similitud):-
  heurística(manhattan),
  estado_meta(EstadoMeta),
  distancia_manhattan(Estado, EstadoMeta, Similitud).

% distancia_manhattan/3
% distancia_manhattan(+Celda1, +Celda, -Distancia)
% Es cierto cuando Distancia unifica con la distancia
% de manhattan entre Pos1 y Pos2.
distancia_manhattan([F1, C1], [F2, C2], Distancia):-
  Distancia is abs(F1 - F2) + abs(C1 - C2).

% distancia_euclidiana/3
% distancia_euclidiana(+Celda1, +Celda2, -Distancia)
% Es cierto cuando Distancia unifica con la distancia
% euclidiana entre Celda1 y Celda2.
distancia_euclidiana([F1, C1], [F2, C2], Distancia):-
  Distancia is sqrt((F1 - F2) ** 2 + (C1 - C2) ** 2).

% inserta/3
% inserta(+Camino, +Frontera, -NuevaFrontera)
% Es cierto cuando NuevaFrontera unifica con una lista
% que contiene a Camino insertado en Frontera según su
% aptitud.
inserta(Camino, [], [Camino]).
inserta(Camino, [Camino2 | RestoCaminos], [Camino, Camino2 | RestoCaminos]):-
  aptitud(Camino, Aptitud1),
  aptitud(Camino2, Aptitud2),
  Aptitud1 =< Aptitud2,!.
inserta(Camino, [Camino2 | RestoCaminos], [Camino2 | RestoCaminos2]):-
  inserta(Camino, RestoCaminos, RestoCaminos2).

% aptitud/2
% aptitud(+Camino, -Aptitud)
% Es cierto cuando Aptitud unifica con la suma de la longitud del camino (costo)
% y la heurística del último estado del camino (similitud). La heurística
% utilizada depende del predicado dinámico heurística/2.
aptitud(Camino, Aptitud):-
  length(Camino, Costo),
  Camino = [Estado | _],
  calcular_heurística(Estado, Similitud),
  Aptitud is Costo + Similitud.

% busca_a_star/3
% busca_a_star(+EstadoInicial, +EstadoMeta, -Plan)
% Es cierto cuando Plan unifica con una lista de estados
% que representa un camino desde el estado inicial hasta
% el estado final.
busca_a_star(EstadoInicial, EstadoMeta, Plan):-
  retractall(estado_meta(_)),
  assert(estado_meta(EstadoMeta)),
  astar([[EstadoInicial]], P),
  reverse(P, Plan).

% astar/2
% astar(+ListaDeListasDeEstados, -Solución)
% Es cierto cuando Solución unifica con una lista de estados
% que representa un camino desde el estado inicial hasta el
% estado final.
astar([[Estado | Camino] | _], [Estado | Camino]):-
  estado_meta(Estado).
astar([[Estado | Camino ] | Frontera], Solución):-
  expandir_nodo(Estado, ListaSucesores),
  inserta_caminos_sucesores(ListaSucesores, [Estado | Camino], Frontera, NuevaFrontera),
  astar(NuevaFrontera, Solución).

% inserta_caminos_sucesores/4
% inserta_caminos_sucesores(+ListaSucesores, +Camino, +Frontera, -NuevaFrontera)
% Es cierto cuando NuevaFrontera unifica con una lista que contiene a los
% caminos de ListaSucesores insertados en Frontera según su aptitud. Los
% caminos que contengan estados repetidos no se insertan.
inserta_caminos_sucesores([], _, Frontera, Frontera).
inserta_caminos_sucesores([Sucesor | RestoSucesores], Camino, Frontera, NuevaFrontera):-
  \+ member(Sucesor, Camino),
  inserta([Sucesor | Camino], Frontera, NuevaFrontera2),
  inserta_caminos_sucesores(RestoSucesores, Camino, NuevaFrontera2, NuevaFrontera).
inserta_caminos_sucesores([_ | RestoSucesores], Camino, Frontera, NuevaFrontera):-
  inserta_caminos_sucesores(RestoSucesores, Camino, Frontera, NuevaFrontera).

% prueba_a_star/0
% Prueba de A* con heurística/1.
prueba_a_star:-
  estado_inicial(EstadoInicial),
  estado_meta(EstadoMeta),
  busca_a_star(EstadoInicial, EstadoMeta, Solución),
  format('Solución A*: ~w~n', [Solución]).

