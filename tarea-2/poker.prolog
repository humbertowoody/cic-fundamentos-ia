% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
% 
% Tarea 2 - Figuras del Póker.
%           Construir un programa en Prolog que genere un mazo de cartas,
%           reparta 5 cartas a 4 jugadores, evalúe las manos y muestre los
%           resultados.
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

% Casino más cercano al CIC para validar esta tarea.
casino_más_cercano_al_cic('Yak Lindavista').

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
  % comodines!
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
  generar_manos_jugadores(20, 5, M, Manos),
  format('Manos generadas:~n'),
  imprimir_lista_de_listas(Manos),
  format('Evaluando manos...~n'),
  maplist(figura_maxima, Manos, Figuras),
  format('Resultado:\n',[]),
  format('Lugar Jugador   Figura        Mano\n',[]),
  format('==========================================================\n',[]),
  imprimir_resultados(Manos, Figuras),
  !.

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

% figura_maxima/2
% figura_maxima(+Mano, -Figura)
% Figura es la figura máxima de la Mano
figura_maxima(Mano, "Flor Imperial"):-
  figura_flor_imperial(Mano).
figura_maxima(Mano, "Flor"):-
  figura_flor(Mano).
figura_maxima(Mano, "Poker"):-
  figura_poker(Mano).
figura_maxima(Mano, "Full"):-
  figura_full(Mano).
figura_maxima(Mano, "Color"):-
  figura_color(Mano).
figura_maxima(Mano, "Escalera"):-
  figura_escalera(Mano). 
figura_maxima(Mano, "Tercia"):-
  figura_tercia(Mano).
figura_maxima(Mano, "Doble Par"):-
  figura_doble_par(Mano).
figura_maxima(Mano, "Par"):-
  figura_par(Mano).
figura_maxima(_, "Nada"):- % Si no es ninguna de las anteriores, entonces es Nada.
  format('Nada~n', []).

% imprimir_resultados/2
% imprimir_resultados(+Manos, +Figuras)
% Imprime los resultados de las manos.
imprimir_resultados([], []).
imprimir_resultados([Mano | RestoM], [Figura | RestoF]):-
  length(RestoM, L),
  Posicion is 4 - L,
  format('~|~w~5+~| ~|~w~9+ ~|~w~13+ ~w~n', [Posicion, "Jugador-1", Figura, Mano]),
  imprimir_resultados(RestoM, RestoF).


% figura_par/1 
% figura_par(+Mano) 
% Mano es una lista de cartas que contiene un par
figura_par(Mano):-
  member(V-_, Mano), select(V-_, Mano, Resto), 
  member(V2-_, Resto), V == V2, 
  member(V3-_, Resto), member(V4-_, Resto), member(V5-_, Resto),
  V3 \== V4, V3 \== V5, V4 \== V5,
  V3 \== V, V4 \== V, V5 \== V,
  format('Par de ~w~n', [V]),
  !.

% figura_tercia/1 
% figura_tercia(+Mano) 
% Mano es una lista de cartas que contiene una tercia
figura_tercia(Mano):-
  member(V-_, Mano), select(V-_, Mano, Resto),
  member(V2-_, Resto), V2 == V, select(V2-_, Resto, Resto2),
  member(V3-_, Resto2), V3 == V, 
  member(V4-_, Resto2), member(V5-_, Resto2),
  V4 \== V5, V4 \== V, V5 \== V,
  format('Tercia de ~w~n', [V]),
  !.

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
  format('Doble par de ~w y ~w~n', [V, V3]),
  !.

% figura_escalera/1 
% figura_escalera(+Mano)
% Mano es una lista de cartas que contiene una escalera
figura_escalera(Mano):-
  Mano = [Carta1, Carta2, Carta3, Carta4, Carta5],
  carta_consecutiva_a(Carta1, Carta2),
  carta_consecutiva_a(Carta2, Carta3),
  carta_consecutiva_a(Carta3, Carta4),
  carta_consecutiva_a(Carta4, Carta5),
  format('Escalera~n', []),
  !.

% figura_color/1 
% figura_color(+Mano) 
% Mano es una lista de cartas que contiene un color
figura_color(Mano):-
  mismo_palo(Mano),
  format('Color~n', []),
  !.

% figura_full/1
% figura_full(+Mano)
% Mano es una lista de cartas que contiene un full
figura_full(Mano):-
  figura_tercia(Mano),
  figura_par(Mano),
  format('Full~n', []),
  !.

% figura_poker/1 
% figura_poker(+Mano) 
% Mano es una lista de cartas que contiene un poker
figura_poker(Mano):-
  Mano = [Carta1, Carta2, Carta3, Carta4, Carta5],
  Carta1 = V-_, Carta2 = V-_, Carta3 = V-_, Carta4 = V-_, Carta5 = V-_,
  format('Poker de ~w~n', [V]),
  !.

% figura_flor/1 
% figura_flor(+Mano) 
% Mano es una lista de cartas que contiene una flor
figura_flor(Mano):-
  mismo_palo(Mano),
  figura_escalera(Mano),
  format('Flor~n', []),
  !.

% figura_flor_imperial/1 
% figura_flor_imperial(+Mano) 
% Mano es una lista de cartas que contiene una flor imperial
figura_flor_imperial(Mano):-
  mismo_palo(Mano),
  figura_escalera(Mano),
  Mano = [Carta1 | _],
  Carta1 = V-_, 
  V == 10,
  format('Flor Imperial~n', []),
  !.

% carta_consecutiva_a/2
% carta_consecutiva_a(+Carta1, +Carta2)
% Carta1 es consecutiva a Carta2 si su valor es consecutivo.
carta_consecutiva_a(Carta1, Carta2):- % Este es el caso dónde ambas cartas son números.
  Carta1 = V1-_, Carta2 = V2-_,
  integer(V1), integer(V2),
  V1 > V2,
  V1 is V2 + 1.
carta_consecutiva_a(Carta1, Carta2):- % Este es el caso dónde Carta1 es un personaje y Carta2 es un numero.
  Carta1 = V1-_, Carta2 = V2-_,
  personaje(V1), integer(V2),
  V1 == 'J',
  V2 == 10.
carta_consecutiva_a(Carta1, Carta2):- % Este es el caso dónde ambas cartas son personajes.
  Carta1 = V1-_, Carta2 = V2-_,
  personaje(V1), personaje(V2),
  lista_personajes(L),
  nth0(Indice1, L, V1),
  nth0(Indice2, L, V2),
  Indice1 > Indice2,
  Indice1 is Indice2 + 1.

