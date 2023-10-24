% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Tarea 2 - Figuras del Póker.
%           Construir un programa en Prolog que genere un mazo de cartas,
%           reparta 5 cartas a 4 jugadores, evalúe las manos y muestre los
%           resultados.
%
% Propuesta de Ejecución:
%   1. Ejecutar el predicado reparte_cartas/0.
%   2. El programa generará un mazo de cartas, repartirá 5 cartas a 4 jugadores,
%      evaluará las manos y mostrará los resultados.
%   3. El programa terminará, si se desea jugar otra partida, ejecutar el predicado
%      reiniciar/0.
%
% Fundamentos de Inteligencia Artificial, CIC, IPN, 2023.
% =============================================================================


% =============================================================================
% Hechos
% =============================================================================

% Lista con los palos disponibles, en el orden de prioridad de la baraja.
lista_palos(['♠️','♥️','♣️','♦️']).

% Lista con los personajes disponibles, en el orden de prioridad de la baraja.
lista_personajes(['A','K','Q','J']).

% Los valores numéricos asociados a cada figura (puntaje) para la evaluación final.
valor_figura("Nada", 1).
valor_figura("Par", 2).
valor_figura("Doble Par", 3).
valor_figura("Tercia", 4).
valor_figura("Escalera", 5).
valor_figura("Color", 6).
valor_figura("Full", 7).
valor_figura("Poker", 8).
valor_figura("Flor", 9).
valor_figura("Flor Imperial", 10).

% =============================================================================
% Predicados
% =============================================================================

% palo/1
% palo(-Palo)
% Palo es un palo de la baraja
palo(P):- lista_palos(L),member(P, L).

% personaje/1
% personaje(-Personaje)
% Personaje es un personaje de la baraja
personaje(P):- lista_personajes(L),member(P, L).

% valor/1
% valor(-Valor)
% Valor es un valor de carta
valor(V):- between(2,10,V).

% comodín/1
% comodín(-Comodín)
% Comodín es una carta comodín del mazo
comodín(C):- member(C,['JK1','JK2','JK3','JK4']).

% carta_personaje/1
% carta_personaje(-Carta)
% Carta es una carta de personaje
carta_personaje(Personaje-Palo):-
  personaje(Personaje),
  palo(Palo).

% carta_valor/1
% carta_valor(-Carta)
% Carta es una carta de valor
carta_valor(Valor-Palo):-
  valor(Valor),
  palo(Palo).

% baraja/1
% baraja(-Baraja)
% Baraja es una lista con todas las cartas de la baraja (sin comodines).
baraja(B):- findall(C, carta_personaje(C), Personajes),
            findall(C, carta_valor(C), Valores),
            append(Personajes, Valores, B).

% barajar/2
% barajar(+Baraja1, -Baraja2)
% Baraja2 es una baraja con las mismas cartas que Baraja1 pero en orden aleatorio
barajar(Baraja1, Baraja2):- random_permutation(Baraja1, Baraja2).

% carta_al_azar/2
% carta_al_azar(+Mazo, -Carta)
% Carta es una carta al azar de Mazo
carta_al_azar(Mazo, Carta, NuevoMazo):-
  [Carta | NuevoMazo] = Mazo.

% mazo/1
% mazo(-Mazo)
% Mazo es un mazo de cartas barajado con comodines
mazo(Mazo):-
  baraja(A),
  baraja(B),
  append(A,B, MazoOrdenado),
  % Esta parte es para agregar los comodines al mazo, pero no se agregan porque
  % no pude hacer que las figuras se evaluaran correctamente con los comodines.
  % findall(C, comodín(C), Comodines),
  % append(Comodines, MazoOrdenado, MazoOrdenadoConComodines),
  % barajar(MazoOrdenadoConComodines, Mazo).
  barajar(MazoOrdenado, Mazo).

% generar_manos_jugadores/4
% generar_manos_jugadores(+NumeroJugadores, +CartasPorJugador, +Mazo, -Manos)
% Manos es una lista de listas de cartas de tamaño NumeroJugadores, cada una de tamaño CartasPorJugador
generar_manos_jugadores(0, _, _, []).
generar_manos_jugadores(NumeroJugadores, CartasPorJugador, Mazo, [Mano | RestoManos]):-
  NumeroJugadores > 0,
  generar_mano_jugador(CartasPorJugador, Mazo, Mano, NuevoMazo),
  NuevoNumeroJugadores is NumeroJugadores - 1,
  generar_manos_jugadores(NuevoNumeroJugadores, CartasPorJugador, NuevoMazo, RestoManos).

% generar_mano_jugador/4
% generar_mano_jugador(+CartasPorJugador, +Mazo, -Mano, -NuevoMazo)
% Mano es una lista de CartasPorJugador cartas al azar de Mazo, NuevoMazo es Mazo sin las cartas de Mano
generar_mano_jugador(0, Mazo, [], Mazo).
generar_mano_jugador(CartasPorJugador, Mazo, [Carta | RestoMano], NuevoMazo):-
  CartasPorJugador > 0,
  carta_al_azar(Mazo, Carta, MazoSinCarta),
  CartasPorJugadorNuevo is CartasPorJugador - 1,
  generar_mano_jugador(CartasPorJugadorNuevo, MazoSinCarta, RestoMano, NuevoMazo).

% reparate_cartas/0
% reparate_cartas()
% Genera un mazo de cartas, reparte 5 cartas a 4 jugadores, evalúa las manos y muestra los resultados
reparte_cartas():-
  format('Generando mazo inicial...~n'),
  mazo(M),
  format('Generando mano de cada jugador...~n'),
  generar_manos_jugadores(4, 5, M, Manos),
  Manos = [Mano1, Mano2, Mano3, Mano4],
  format('Evaluando manos...~n'),
  figura_máxima(Mano1, Figura1),
  figura_máxima(Mano2, Figura2),
  figura_máxima(Mano3, Figura3),
  figura_máxima(Mano4, Figura4),
  Jugadores = [
    [1, Mano1, Figura1],
    [2, Mano2, Figura2],
    [3, Mano3, Figura3],
    [4, Mano4, Figura4]
  ],
  ordenar_jugadores(Jugadores, JugadoresOrdenados),
  format('Resultado:\n',[]),
  format('Lugar Jugador   Figura        Mano\n',[]),
  format('==========================================================\n',[]),
  imprimir_resultados(JugadoresOrdenados),
  !.

% reiniciar/0
% reiniciar()
% Reinicia el programa
reiniciar():-
  format('Reiniciando programa...~n'),
  reparte_cartas().

% comparar_jugador/3
% comparar_jugador(+Dif, +Jugador1, +Jugador2)
% Dif es el resultado de comparar Jugador1 y Jugador2
comparar_jugador(Dif, [ID1, _, Figura1], [ID2, _, Figura2]) :-
    valor_figura(Figura1, Valor1),
    valor_figura(Figura2, Valor2),
    (   Valor1 = Valor2
    ->  compare(Dif, ID1, ID2)  % Usa el ID como desempate si las manos son iguales
    ;   compare(Dif, Valor2, Valor1)  % De lo contrario, compara basándose en el valor de la mano
    ).

% ordenar_jugadores/2
% ordenar_jugadores(+Jugadores, -JugadoresOrdenados)
% JugadoresOrdenados es una lista de jugadores ordenados de mayor a menor puntaje según valor_figura/1.
ordenar_jugadores(Jugadores, JugadoresOrdenados) :-
    predsort(comparar_jugador, Jugadores, JugadoresOrdenados).

% imprimir_lista_de_listas/1
% imprimir_lista_de_listas(+ListaDeListas)
% Imprime una lista de listas
imprimir_lista_de_listas([]).
imprimir_lista_de_listas([Lista|Resto]) :-
    write(Lista), nl,
    imprimir_lista_de_listas(Resto).

% mismo_palo/1
% mismo_palo(+Mano)
% Mano es una lista de cartas del mismo palo
mismo_palo(Mano):-
  Mano = [_-Palo, _-Palo, _-Palo, _-Palo, _-Palo].

% figura_máxima/2
% figura_máxima(+Mano, -Figura)
% Figura es la figura máxima de la Mano, se aprovecha del orden de evaluación del motor de inferencia.
figura_máxima(Mano, "Flor Imperial"):-
  figura_flor_imperial(Mano).
figura_máxima(Mano, "Flor"):-
  figura_flor(Mano).
figura_máxima(Mano, "Poker"):-
  figura_poker(Mano).
figura_máxima(Mano, "Full"):-
  figura_full(Mano).
figura_máxima(Mano, "Color"):-
  figura_color(Mano).
figura_máxima(Mano, "Escalera"):-
  figura_escalera(Mano).
figura_máxima(Mano, "Tercia"):-
  figura_tercia(Mano).
figura_máxima(Mano, "Doble Par"):-
  figura_doble_par(Mano).
figura_máxima(Mano, "Par"):-
  figura_par(Mano).
figura_máxima(_, "Nada"):- % Si no es ninguna de las anteriores, entonces es Nada.
  format('- Nada~n', []).

% imprimir_resultados/2
% imprimir_resultados(+Manos, +Figuras)
% Imprime los resultados de las manos.
imprimir_resultados([]).
imprimir_resultados([Jugador | Resto]):-
  length(Resto, L),
  Posicion is 4 - L,
  Jugador = [ID, Mano, Figura],
  format('~|~w~5+~| ~|Jugador-~w~9+ ~|~w~13+ ~w~n', [Posicion, ID, Figura, Mano]),
  imprimir_resultados(Resto).


% figura_par/1
% figura_par(+Mano)
% Mano es una lista de cartas que contiene un par
figura_par(Mano):-
  member(V-_, Mano), select(V-_, Mano, Resto),
  member(V2-_, Resto), V == V2,
  member(V3-_, Resto), member(V4-_, Resto), member(V5-_, Resto),
  V3 \== V4, V3 \== V5, V4 \== V5,
  V3 \== V, V4 \== V, V5 \== V,
  format('- Par de ~w~n', [V]).

% figura_doble_par/1
% figura_doble_par(+Mano)
% Mano es una lista de cartas que contiene un doble par
figura_doble_par(Mano):-
  member(V-_, Mano), select(V-_, Mano, Resto),
  member(V2-_, Resto), V2 == V, select(V2-_, Resto, Resto2),
  member(V3-_, Resto2), select(V3-_, Resto2, Resto3),
  member(V4-_, Resto3), V4 == V3, select(V4-_, Resto3, Resto4),
  member(V5-_, Resto4),
  V5 \== V, V5 \== V3,
  V \== V3,
  format('- Doble par de ~w y ~w~n', [V, V3]).

% figura_tercia/1
% figura_tercia(+Mano)
% Mano es una lista de cartas que contiene una tercia
figura_tercia(Mano):-
  member(V-_, Mano), select(V-_, Mano, Resto),
  member(V2-_, Resto), V2 == V, select(V2-_, Resto, Resto2),
  member(V3-_, Resto2), V3 == V,
  member(V4-_, Resto2), member(V5-_, Resto2),
  V4 \== V5, V4 \== V, V5 \== V,
  format('- Tercia de ~w~n', [V]).

% figura_escalera/1
% figura_escalera(+Mano)
% Mano es una lista de cartas que contiene una escalera
figura_escalera(Mano):-
  member(Carta1, Mano), select(Carta1, Mano, Resto),
  member(Carta2, Resto), carta_consecutiva_a(Carta2, Carta1),select(Carta2, Resto, Resto2),
  member(Carta3, Resto2), carta_consecutiva_a(Carta3, Carta2),select(Carta3, Resto2, Resto3),
  member(Carta4, Resto3), carta_consecutiva_a(Carta4, Carta3),select(Carta4, Resto3, Resto4),
  member(Carta5, Resto4), carta_consecutiva_a(Carta5, Carta4),
  format('- Escalera~n', []).

% figura_color/1
% figura_color(+Mano)
% Mano es una lista de cartas que contiene un color
figura_color(Mano):-
  mismo_palo(Mano),
  format('- Color~n', []).

% figura_full/1
% figura_full(+Mano)
% Mano es una lista de cartas que contiene un full
figura_full(Mano):-
  member(V-_, Mano), select(V-_, Mano, Resto),
  member(V2-_, Resto), V2 == V, select(V2-_, Resto, Resto2),
  member(V3-_, Resto2), V3 == V,  select(V3-_, Resto2, Resto3),
  member(V4-_, Resto3), select(V4-_, Resto3, Resto4),
  member(V5-_, Resto4), V5 == V4,
  V5 \== V,
  format('- Full~n', []).

% figura_poker/1
% figura_poker(+Mano)
% Mano es una lista de cartas que contiene un poker
figura_poker(Mano):-
  member(V-_, Mano), select(V-_, Mano, Resto),
  member(V2-_, Resto), V2 == V, select(V2-_, Resto, Resto2),
  member(V3-_, Resto2), V3 == V, select(V3-_, Resto2, Resto3),
  member(V4-_, Resto3), V4 == V, select(V4-_, Resto3, Resto4),
  member(V5-_, Resto4), V5 \== V,
  format('- Poker de ~w~n', [V]).

% figura_flor/1
% figura_flor(+Mano)
% Mano es una lista de cartas que contiene una flor
figura_flor(Mano):-
  mismo_palo(Mano),
  figura_escalera(Mano),
  format('- Flor~n', []).

% figura_flor_imperial/1
% figura_flor_imperial(+Mano)
% Mano es una lista de cartas que contiene una flor imperial
figura_flor_imperial(Mano):-
  mismo_palo(Mano),
  figura_escalera(Mano),
  member(10-_, Mano),
  member('A'-_, Mano),
  format('- Flor Imperial~n', []).

% carta_consecutiva_a/2
% carta_consecutiva_a(+Carta1, +Carta2)
% Carta1 es consecutiva a Carta2 si su valor es consecutivo.
carta_consecutiva_a(V1-_, V2-_):- % Este es el caso dónde ambas cartas son números.
  integer(V1), integer(V2),
  V1 > V2,
  V1 is (V2 + 1), !.
carta_consecutiva_a('J'-_, 10-_):- % Este es el caso dónde Carta1 es un personaje y Carta2 es un numero.
  !.
carta_consecutiva_a(V1-_, V2-_):- % Este es el caso dónde ambas cartas son personajes.
  personaje(V1), personaje(V2),
  lista_personajes(L),
  nth0(Indice1, L, V1),
  nth0(Indice2, L, V2),
  Indice1 < Indice2,
  Indice2 is Indice1 + 1, !.

% =============================================================================
% Pruebas
% Esta sección solo contiene código utilizado para probar cada una de las figuras,
% puede ser muy útil para depurar el programa.
% =============================================================================

% Estas listas son para hacer pruebas.
%lista_cartas_prueba([10-'♦️', 'J'-'♦️', 'Q'-'♦️', 'K'-'♦️','A'-'♦️']). % Flor Imperial
%lista_cartas_prueba([7-'♥️', 8-'♥️', 9-'♥️', 10-'♥️', 'J'-'♥️']). % Flor
%lista_cartas_prueba(['A'-'♠️', 'A'-'♥️', 'A'-'♣️', 'A'-'♦️', 'K'-'♠️']). % Poker de A
%lista_cartas_prueba(['A'-'♠️', 'A'-'♥️', 'A'-'♣️', 2-'♦️', 2-'♠️']). % Full
%lista_cartas_prueba(['A'-'♠️', 'A'-'♠️', 2-'♠️', 3-'♠️', 'K'-'♠️']). % Color
%lista_cartas_prueba([3-'♠️', 4-'♥️', 5-'♣️', 6-'♦️', 7-'♠️']). % Escalera
%lista_cartas_prueba(['A'-'♠️', 'A'-'♥️', 'A'-'♣️', 2-'♦️', 'K'-'♠️']). % Tercia de A
%lista_cartas_prueba(['A'-'♠️', 'A'-'♥️', 2-'♣️', 2-'♦️', 'K'-'♠️']). % Doble par de A y 2
%lista_cartas_prueba(['A'-'♠️', 'A'-'♥️', 2-'♣️', 3-'♦️', 4-'♠️']). % Par de A

% prueba():-
%   lista_cartas_prueba(L),
%   random_permutation(L, L2),
%   format('Lista de cartas de prueba: ~w~n', [L]),
%   figura_máxima(L, _),
%   format('Lista de cartas de prueba barajada: ~w~n', [L2]),
%   figura_máxima(L2, _),
%   !.
