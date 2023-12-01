

% Consultamos el archivo que contiene el laberinto, el estado inicial y
% el estado final.
:- consult('laberinto-01.prolog').

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

heurística(Estado, Similitud):-
  estado_meta(EstadoMeta),
  % distancia_euclidiana(Estado, EstadoMeta, Similitud).
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

inserta(Camino, [], [Camino]).
inserta(Camino, [Camino2 | RestoCaminos], [Camino, Camino2 | RestoCaminos]):-
  aptitud(Camino, Aptitud1),
  aptitud(Camino2, Aptitud2),
  %format('Camino: ~w~n', [Camino]),
  %format('Camino2: ~w~n', [Camino2]),
  %format('Aptitud1: ~w~n', [Aptitud1]),
  %format('Aptitud2: ~w~n', [Aptitud2]),
  Aptitud1 =< Aptitud2,!.
inserta(Camino, [Camino2 | RestoCaminos], [Camino2 | RestoCaminos2]):-
  inserta(Camino, RestoCaminos, RestoCaminos2).


aptitud(Camino, Aptitud):-
  length(Camino, Costo),
  Camino = [Estado | _],
  heurística(Estado, Similitud),
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
% astar(+ListaDeListasDeEstados, -Solucion)
% Es cierto cuando Solucion unifica con una lista de estados
% que representa un camino desde el estado inicial hasta el
% estado final.
astar([[Estado | Camino] | _], [Estado | Camino]):-
  estado_meta(Estado).
astar([[Estado | Camino ] | Frontera], Solución):-
  expandir_nodo(Estado, ListaSucesores),
  inserta_caminos_sucesores(ListaSucesores, [Estado | Camino], Frontera, NuevaFrontera),
  astar(NuevaFrontera, Solución).

inserta_caminos_sucesores([], _, Frontera, Frontera).
inserta_caminos_sucesores([Sucesor | RestoSucesores], Camino, Frontera, NuevaFrontera):-
  \+ member(Sucesor, Camino),
  %format('Sucesor: ~w~n', [Sucesor]),
  %format('Frontera: ~w~n', [Frontera]),
  inserta([Sucesor | Camino], Frontera, NuevaFrontera2),
  %format('Nueva frontera 2: ~w~n', [NuevaFrontera2]),
  inserta_caminos_sucesores(RestoSucesores, Camino, NuevaFrontera2, NuevaFrontera).
inserta_caminos_sucesores([_ | RestoSucesores], Camino, Frontera, NuevaFrontera):-
  inserta_caminos_sucesores(RestoSucesores, Camino, Frontera, NuevaFrontera).



prueba:-
  estado_inicial(EstadoInicial),
  estado_meta(EstadoMeta),
  busca_a_star(EstadoInicial, EstadoMeta, Solucion),
  write(Solucion).
