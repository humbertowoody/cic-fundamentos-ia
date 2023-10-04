%=======
% Tarea 2 - Humberto Alejandro Ortega Alcocoer - 2016630495
%====


% palo/1
% palo(-Palo)
% Palo es un palo de la baraja
palo(P):- member(P, ['♠️','♥️','♣️','♦️']).

% personaje/1
% personaje(-Personaje)
% Personaje es un personaje de la baraja
personaje(P):- member(P, ['A','J','Q','K']).

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
  findall(C, comodín(C), Comodines),
  append(Comodines, MazoOrdenado, MazoOrdenadoConComodines),
  barajar(MazoOrdenadoConComodines, Mazo).

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
mismo_palo([Carta | Resto]):-
  Carta = _-Palo,
  maplist(=(Palo), Resto).

% tienen_dos_cartas_de_mismo_valor/1 
% tienen_dos_cartas_de_mismo_valor(+Mano) 
% Mano es una lista de cartas que contiene dos cartas de mismo valor
tiene_dos_cartas_de_mismo_valor([Carta | Resto]):-
  Carta = Valor-_,
  member(Carta, Resto).
tienen_tres_cartas_de_mismo_valor([_| Resto]):-
  tiene_dos_cartas_de_mismo_valor(Resto).

% figura_maxima/2
% figura_maxima(+Mano, -Figura)
% Figura es la figura máxima de la Mano
figura_maxima(Mano, "Flor Imperial"):-
  mismo_palo(Mano),
  !.
figura_maxima(Mano, "Flor"):-
  mismo_palo(Mano), !.
figura_maxima(Mano, "Poker"):-
  Mano = [Carta1, Carta2, Carta3, Carta4, Carta5],
  Carta1 = _-Palo,
  Carta2 = _-Palo, !.
figura_maxima(Mano, "Full"):-
  Mano = [Carta1, Carta2, Carta3, Carta4, Carta5],
  Carta1 = Valor1-_,
  Carta2 = Valor2-_,
  Carta3 = Valor3-_,
  Carta4 = Valor4-_,
  Carta5 = Valor5-_,
  (Valor1 = Valor2; Valor1 = Valor3; Valor1 = Valor4; Valor1 = Valor5),
  (Valor2 = Valor3; Valor2 = Valor4; Valor2 = Valor5),
  (Valor3 = Valor4; Valor3 = Valor5),
  !.
figura_maxima(Mano, "Color"):-
  mismo_palo(Mano), !.
figura_maxima(Mano, "Escalera"):-
  Mano = [Carta1, Carta2, Carta3, Carta4, Carta5],
  Carta1 = Valor1-_,
  Carta2 = Valor2-_,
  Carta3 = Valor3-_,
  Carta4 = Valor4-_,
  Carta5 = Valor5-_,
  Valor1 = Valor2 - 1,
  Valor2 = Valor3 - 1,
  Valor3 = Valor4 - 1,
  Valor4 = Valor5 - 1,
  !.
figura_maxima(Mano, "Trio"):-
  Mano = [Carta1, Carta2, Carta3, Carta4, Carta5],
  Carta1 = Valor1-_,
  Carta2 = Valor2-_,
  Carta3 = Valor3-_,
  Carta4 = Valor4-_,
  Carta5 = Valor5-_,
  (Valor1 = Valor2; Valor1 = Valor3; Valor1 = Valor4; Valor1 = Valor5),
  (Valor2 = Valor3; Valor2 = Valor4; Valor2 = Valor5),
  (Valor3 = Valor4; Valor3 = Valor5),
  !.
figura_maxima(Mano, "Doble Par"):-
  Mano = [Carta1, Carta2, Carta3, Carta4, Carta5],
  Carta1 = Valor1-_,
  Carta2 = Valor2-_,
  Carta3 = Valor3-_,
  Carta4 = Valor4-_,
  Carta5 = Valor5-_,
  (Valor1 = Valor2; Valor1 = Valor3; Valor1 = Valor4; Valor1 = Valor5),
  (Valor2 = Valor3; Valor2 = Valor4; Valor2 = Valor5),
  (Valor3 = Valor4; Valor3 = Valor5),
  !.
% Par 
% Un par es una mano de 5 cartas en la que dos de ellas tienen el mismo valor.
figura_maxima(Mano, "Par"):-
  tiene_dos_cartas_de_mismo_valor(Mano);
  (comodín(C), member(C, Mano)),
  !.
% Predicado por defecto, el motor de inferencia llegará a este punto (por el corte)
% si no se cumple ninguna de las condiciones anteriores.
figura_maxima(_, "Nada").

% imprimir_resultados/2
% imprimir_resultados(+Manos, +Figuras)
% Imprime los resultados de las manos.
imprimir_resultados([], []).
imprimir_resultados([Mano | RestoM], [Figura | RestoF]):-
  format('~|~w~5+ ~|~w~9+ ~|~w~13+ ~w~n', ["ñ", "Jugador-1", Figura, Mano]),
  imprimir_resultados(RestoM, RestoF).
