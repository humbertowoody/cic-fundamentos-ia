% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Proyecto Final - Agente Jugador de Mancala
%                  Implementación de un agente jugador de Mancala usando el algoritmo
%                  negamax con podas Alpha-Beta y una heurística propia. Para jugar,
%                  se debe ejecutar el predicado inicia_juego/1, el cual recibe como
%                  parámetro el horizonte de búsqueda del algoritmo. En cada turno
%                  se colocará el número de la casilla que se desea seleccionar y
%                  el orden en que se repartirán las fichas, por ejemplo, para
%                  repartir una ficha de cada color se puede escribir avr, rva, arv,
%                  entre otros. El juego termina cuando un jugador no puede hacer
%                  jugadas válidas.
%
% Propuesta de Ejecución:
%
% ?- inicia_juego(Horizonte). % Horizonte = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10.
%                             % El horizonte indica la profundidad de búsqueda del
%                             % algoritmo negamax con podas Alpha-Beta.
%
% Fundamentos de Inteligencia Artificial, CIC, IPN, 2024.
% =============================================================================

% Librería para opciones gráficas.
:- use_module(library(ansi_term)).

% Librería CLPFD.
:- use_module(library(clpfd)).

% tablero_vacío/1
% tablero_vacío(-Tablero)
% Tablero es una lista de 14 casillas vacías.
tablero_vacío(Tablero) :-
    Tablero =   [[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0], % Jugador 1
                [0,0,0],                                          % Base Jugador 1
                [0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],  % Jugador 2
                [0,0,0]].                                         % Base Jugador 2

% tablero_inicial/1
% tablero_inicial(-Tablero)
% Tablero es una lista de 14 casillas con 3 fichas amarillas, 3 verdes y 3 rojas
% en cada casilla de los jugadores y 0 fichas en las bases.
tablero_inicial(Tablero) :-
    Tablero =   [[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1], % Casillas Jugador 1
                [0,0,0],    % Base Jugador 1
                [1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1],[1,1,1],  % Casillas Jugador 2
                [0,0,0]].   % Base Jugador 2

% ficha_aumentar/3
% ficha_aumentar(+Color,+Casilla,-NuevaCasilla)
% NuevaCasilla es una lista con una ficha más del color indicado en la casilla indicada.
ficha_aumentar(x,Casilla,Casilla).
ficha_aumentar(Color,Casilla,NuevaCasilla) :-
    Color \= x,
    ((Color = a, Posición = 1);
    (Color = v, Posición = 2);
    (Color = r, Posición = 3)),
    nth1(Posición,Casilla,Cantidad,Resto),
    nth1(Posición,NuevaCasilla,NuevaCantidad,Resto),
    NuevaCantidad #= Cantidad + 1.

% modificar_tablero/4
% modificar_tablero(+Casilla,+Jugada,+Tablero,-NuevoTablero)
% NuevoTablero es el tablero resultante de aplicar la Jugada a la Casilla del Tablero.
modificar_tablero(Casilla,Jugada,Tablero,NuevoTablero) :-
    % Permite hacer jugadas con casillas que contengan más de 12 fichas
    TamañoJugada #> 13,
    length(Jugada,TamañoJugada),
    TamañoExtra #= TamañoJugada - 13,
    length(Extra,TamañoExtra),
    append(NuevaJugada,Extra,Jugada),
    modificar_tablero(Casilla,Extra,Tablero,NT),
    modificar_tablero(Casilla,NuevaJugada,NT,NuevoTablero).
modificar_tablero(Casilla,Jugada,Tablero,NuevoTablero) :-
    TamañoJugada #< 14,
    % Agrega x para hacer que la lista de jugadas coincida con el tamaño
    % del tablero para poder emplear maplist/4
    length(Tablero,TamañoTablero),
    length(Jugada,TamañoJugada),
    TamañoRelleno #= TamañoTablero - TamañoJugada,
    length(Relleno, TamañoRelleno),
    maplist(=(x),Relleno),
    append(Jugada,Relleno,JugadaCompleta),
    % Divide el tablero en dos secciones y las intercambia de lugar para
    % que las casillas que reciben fichas queden al principio del tablero
    length(T1,Casilla),
    append(T1,T2,Tablero),
    append(T2,T1,TableroReordenado),
    maplist(ficha_aumentar,JugadaCompleta,TableroReordenado,Resultado),
    % Regresa el tablero (ya modificado) a su orden original
    length(T4,Casilla),
    append(T3,T4,Resultado),
    append(T4,T3,NuevoTablero).

% jugada/5
% jugada(+JugadorEnTurno,+Casilla,+Jugada,+Tablero,-NuevoTablero)
% NuevoTablero es el tablero resultante de aplicar la Jugada a la Casilla del Tablero.
jugada(JugadorEnTurno,Casilla,Jugada,Tablero,NuevoTablero) :-
    Casilla in 1..6,
    Jugada = [_|_],
    Tablero = [C1,C2,C3,C4,C5,C6, BaseJ1, C7,C8,C9,C10,C11,C12, BaseJ2],
    ((JugadorEnTurno = 1, T1 = [C1,C2,C3,C4,C5,C6,BaseJ1,C7,C8,C9,C10,C11,C12]) ;
    (JugadorEnTurno = 2, T1 = [C7,C8,C9,C10,C11,C12,BaseJ2,C1,C2,C3,C4,C5,C6])),
    nth1(Casilla,T1,[Amarillas,Verdes,Rojas]),
    CantidadFichas #= Amarillas + Verdes + Rojas,
    length(Jugada,CantidadFichas),
    contar_fichas(Jugada,a,Amarillas),
    contar_fichas(Jugada,v,Verdes),
    contar_fichas(Jugada,r,Rojas),
    nth1(Casilla,T1,_,Resto),
    nth1(Casilla,T,[0,0,0],Resto),
    modificar_tablero(Casilla,Jugada,T,NT),
    ((JugadorEnTurno = 1, append(NT,[BaseJ2],NuevoTablero)) ;
    (JugadorEnTurno = 2, NT = [NC7,NC8,NC9,NC10,NC11,NC12,NBaseJ2,NC1,NC2,NC3,NC4,NC5,NC6],
    NuevoTablero = [NC1,NC2,NC3,NC4,NC5,NC6,BaseJ1,NC7,NC8,NC9,NC10,NC11,NC12,NBaseJ2])).

% siguiente_turno/4
% siguiente_turno(+Casilla,+TamañoJugada,+JugadorEnTurno,-JugadorSiguiente)
% JugadorSiguiente es el jugador que le toca el siguiente turno.
siguiente_turno(Casilla,TamañoJugada,JugadorEnTurno,JugadorEnTurno) :-
    7 - Casilla #= TamañoJugada mod 13, !.
siguiente_turno(_,_,JugadorEnTurno,JugadorSiguiente) :-
    JugadorEnTurno = 1
    -> JugadorSiguiente = 2 ; JugadorSiguiente = 1.

% contar_fichas/3
% contar_fichas(+Lista,+Ficha,-Cantidad)
% Cantidad es el número de fichas de un color en la lista.
contar_fichas([],_,0).
contar_fichas([X|Resto],X,Y) :-
    contar_fichas(Resto,X,Z),
    Y is 1+Z.
contar_fichas([Otro|Resto],X,Z):-
    Otro \= X,
    contar_fichas(Resto,X,Z).

% inicia_juego/1
% inicia_juego(+Horizonte)
% Inicia el juego con el horizonte de búsqueda indicado.
inicia_juego(H):-
    retractall(horizonte(_)),
    assert(horizonte(H)),
    % Saludar y mostrar tablero inicial
    ansi_format([bold,fg(green)],'~n~tAgente jugador de Mancala~n',[]),
    format('
    El tradicional juego de Mancala, también conocido como Kalah, es un juego de mesa '),
    format('
    de origen africano que consiste en mover fichas entre casillas con el objetivo de '),
    format('
    obtener la mayor cantidad de puntos. Para ver un video de cómo se juega puedes '),
    format('
    visitar https://youtu.be/OX7rj93m6o8?si=F1G_BzA3KdGAdwQP. El agente será el '),
    ansi_format([fg(blue),bold],'jugador 1',[]),
    format('
    y tú eres el '),
    ansi_format([fg(magenta),bold],'jugador 2',[]),
    format('.
    '),
    format('
    En su turno, debe indicar el número de la casilla que desea seleccionar. A continuación,
    se le mostrarán las fichas disponibles y deberá especificar el orden en el que se deben
    repartir, utilizando una secuencia de letras minúsculas sin espacios. Cada letra representa
    una ficha de un color específico: "a" para amarillo, "v" para verde y "r" para rojo.

    Por ejemplo, para distribuir una ficha de cada color, puede ingresar secuencias como "avr",
    "rva", "arv" u otras combinaciones similares.~n'),
    tablero_inicial(TableroInicial),
    once(imprime_tablero(TableroInicial)),
    % Comenzar juego
    jugar(2,TableroInicial).

% jugar/2
% jugar(+JugadorEnTurno,+Tablero)
% JugadorEnTurno es el jugador que le toca el siguiente turno.
jugar(_,Tablero) :-
    fin(Tablero),
    calcula_puntajes(Tablero,_,_),
    !.
jugar(1,Tablero) :- % Turno del jugador 1 (agente)
    % Encontrar mejor jugada posible
    once(tirar(Tablero,Casilla,Jugada)),
    jugada(1,Casilla,Jugada,Tablero,NT),
    % Anunciar jugada
    imprime_jugada(Casilla,Jugada),
    % Averiguar a quién le toca el siguiente turno
    length(Jugada,TamañoJugada),
    siguiente_turno(Casilla,TamañoJugada,1,JugadorSiguiente),
    % Imprimir tablero sólo si no repite turno
    ((JugadorSiguiente = 1,
    format('
    El agente juega de nuevo~n')) ;
    (JugadorSiguiente = 2,
    once(imprime_tablero(NT)))),
    jugar(JugadorSiguiente,NT).
jugar(2,Tablero) :- % Turno del jugador 2 (humano)
    format('
    Escoge una casilla (1-6): ~t'),
    read_line_to_string(user_input,StringCasilla),
    StringCasilla \= "s",
    casilla_válida(StringCasilla,Casilla),
    obtener_casilla(Tablero,Casilla,Fichas,CasillaVálida),
    format('
    La casilla que escogiste tiene ~w fichas amarillas, ~w verdes y ~w rojas.~n
    Indica en qué orden deseas que se coloquen, no utilices espacios: ',Fichas),
    read_line_to_string(user_input,StringJugada),
    atom_string(Jugada,StringJugada),
    jugada_válida(Jugada,Fichas,ListaJugada),
    once(jugada(2,CasillaVálida,ListaJugada,Tablero,NT)),
    once(imprime_tablero(NT)),
    % Averiguar a quién le toca en el siguiente turno
    length(ListaJugada,TamañoJugada),
    siguiente_turno(Casilla,TamañoJugada,2,JugadorSiguiente),
    ((JugadorSiguiente = 1,
    format('
    Turno del agente jugador.~n')) ;
    (JugadorSiguiente = 2,
    format('
    Es tu turno nuevamente~n'))),
    jugar(JugadorSiguiente,NT).

% casilla_válida/2
% casilla_válida(+StringCasilla,-Casilla)
% Casilla es el número de casilla que se le indica al jugador.
casilla_válida(StringCasilla,Casilla) :-
    number_string(Casilla,StringCasilla),
    StringCasilla \= "s",
    Casilla in 0..6,!.
casilla_válida(StringCasilla,Casilla) :-
    StringCasilla \= "s",
    format('
    Selecciona una casilla válida (1-6): ~t'),
    read_line_to_string(user_input,NuevaString),
    casilla_válida(NuevaString,Casilla).

% obtener_casilla/4
% obtener_casilla(+Tablero,+Casilla,-Fichas,-CasillaVálida)
% Fichas es la lista de fichas que contiene la casilla indicada, CasillaVálida es la casilla
% que se le indica al jugador.
obtener_casilla(Tablero,Casilla,Fichas,Casilla) :-
    ÍndiceCasilla #= Casilla + 7,
    nth1(ÍndiceCasilla,Tablero,Fichas),
    Fichas = [Amarillas,Verdes,Rojas],
    CantidadFichas #= Amarillas + Verdes + Rojas,
    CantidadFichas #> 0,!.
obtener_casilla(Tablero,_,Fichas,CasillaVálida) :-
    format('
    Por favor escoge una casilla válida (1-6) que no esté vacía: ~t'),
    read_line_to_string(user_input,NuevaString),
    casilla_válida(NuevaString,Casilla),
    obtener_casilla(Tablero,Casilla,Fichas,CasillaVálida).

% jugada_válida/3
% jugada_válida(+Jugada,+Fichas,-ListaJugada)
% ListaJugada es la lista de fichas que se colocarán en la casilla, es válida si
% la cantidad de fichas en la lista es igual a la cantidad de fichas en la casilla.
jugada_válida(Jugada,Fichas,ListaJugada) :-
    Fichas = [Amarillas,Verdes,Rojas],
    CantidadFichas #= Amarillas + Verdes + Rojas,
    atom_chars(Jugada,ListaJugada),
    contar_fichas(ListaJugada,a,Amarillas),
    contar_fichas(ListaJugada,v,Verdes),
    contar_fichas(ListaJugada,r,Rojas),
    length(ListaJugada,CantidadFichas),!.
jugada_válida(Jugada,Fichas,ListaJugada) :-
    Jugada \= "s",
    format('
    Por favor ingresa una secuencia válida: ~t'),
    read_line_to_string(user_input,NuevaString),
    jugada_válida(NuevaString,Fichas,ListaJugada).

% fin/1
% fin(+Tablero)
% Verdadero si el juego ha terminado, es decir, si alguno de los jugadores no puede
% hacer jugadas válidas.
fin(Tablero) :-
    V = [0,0,0], % Casilla vacía
    Tablero = [V,V,V,V,V,V|_].
fin(Tablero) :-
    V = [0,0,0], % Casilla vacía
    Tablero = [_,_,_,_,_,_,_, V,V,V,V,V,V,_].

% calcula_puntajes/3
% calcula_puntajes(+Tablero,-PuntajeJ1,-PuntajeJ2)
% PuntajeJ1 y PuntajeJ2 son los puntajes de los jugadores 1 y 2 respectivamente
% en el tablero dado.
calcula_puntajes(Tablero,PuntajeJ1,PuntajeJ2) :-
    V = [0,0,0], % Casilla vacía
    Tablero = [V,V,V,V,V,V, BaseJ1, C7,C8,C9,C10,C11,C12, BaseJ2],
    J1 = [BaseJ1,C7,C8,C9,C10,C11,C12],
    maplist(puntaje_casilla,J1,PuntajesJ1),
    sum_list(PuntajesJ1,PuntajeJ1),
    puntaje_casilla(BaseJ2,PuntajeJ2),
    imprime_resultado(PuntajeJ1,PuntajeJ2).
calcula_puntajes(Tablero,PuntajeJ1,PuntajeJ2) :-
    V = [0,0,0], % Casilla vacía
    Tablero = [C1,C2,C3,C4,C5,C6, BaseJ1, V,V,V,V,V,V, BaseJ2],
    puntaje_casilla(BaseJ1,PuntajeJ1),
    J2 = [BaseJ2,C1,C2,C3,C4,C5,C6],
    maplist(puntaje_casilla,J2,PuntajesJ2),
    sum_list(PuntajesJ2,PuntajeJ2),
    imprime_resultado(PuntajeJ1,PuntajeJ2).

% puntaje_casilla/2
% puntaje_casilla(+Casilla,-Puntaje)
% Puntaje es la cantidad de puntos que vale la casilla para el jugador 1.
puntaje_casilla(Casilla,Puntaje) :-
    Casilla = [Amarillas,Verdes,Rojas],
    Puntaje #= Amarillas + Verdes*5 + Rojas*10.

% horizonte/1
% horizonte(-Horizonte)
% Horizonte es la profundidad de búsqueda del algoritmo negamax con podas Alpha-Beta,
% es dinámico para poder modificarlo en tiempo de ejecución.
:- dynamic(horizonte/1).
horizonte(4). % Profundidad de búsqueda por defecto

% alfa/2
% alfa(-Tablero,-Alfa)
% Alfa es el valor de la poda alpha para el tablero dado, es dinámico para poder
% modificarlo en tiempo de ejecución.
:- dynamic(alfa/2).

% modificaAlfa/2
% modificaAlfa(+Tablero,+Alfa)
% Modifica el valor de la poda alpha para el tablero dado.
modificaAlfa(Tablero,Alfa) :-
    retractall(alfa(Tablero,_)),
    assert(alfa(Tablero,Alfa)).

% mejorJugada/2
% mejorJugada(-MejorJugada,-Aptitud)
% MejorJugada es la mejor jugada posible para el jugador 1 en el tablero dado.
:- dynamic(mejorJugada/2).

% añadir_mejor_jugada/2
% añadir_mejor_jugada(+MejorJugada,+Aptitud)
% Añade la mejor jugada posible al conjunto de mejores jugadas.
añadir_mejor_jugada([Casilla,Jugada],Aptitud) :-
    (
        (\+ mejorJugada([Casilla,Jugada],_));
        ((mejorJugada([Casilla,Jugada],A),
        Aptitud > A),
        retractall(mejorJugada([Casilla,Jugada],_)))
    ),
    assert(mejorJugada([Casilla,Jugada],Aptitud)).

% decision_negamax/3
% decision_negamax(+Tablero,-MejorJugada,-Aptitud)
% MejorJugada es la mejor jugada posible para el jugador 1 en el tablero dado.
decisión_negamax(Tablero,MejorJugada,Aptitud) :-
    MejorJugada = [Casilla,Jugada],
    retractall(mejorJugada(_,_)),
    modificaAlfa(Tablero,-200),
    generar_jugada(Casilla,Tablero,Jugada),
    jugada(1,Casilla,Jugada,Tablero,NuevoTablero),
    alfa(Tablero,A),
    modificaAlfa(NuevoTablero,A),
    length(Jugada,TamañoJugada),
    siguiente_turno(Casilla,TamañoJugada,1,JugadorSiguiente),
    valor_negamax(NuevoTablero,JugadorSiguiente,0,Aptitud),
    añadir_mejor_jugada(MejorJugada,Aptitud).

% valor_negamax/4
% valor_negamax(+Tablero,+Jugador,+Profundidad,-Aptitud)
% Aptitud es la diferencia entre los puntos del jugador 1 y el jugador 2, se
% calcula con el algoritmo negamax con podas Alpha-Beta.
valor_negamax(Tablero,_,Profundidad,Aptitud) :-
    (fin(Tablero) ; horizonte(Profundidad)),
    aptitud(Tablero,Aptitud),!.
valor_negamax(Tablero,Jugador,Profundidad,Aptitud) :-
    % Encuentra los descendientes
    generar_jugada(Casilla,Tablero,Jugada),
    jugada(Jugador,Casilla,Jugada,Tablero,NuevoTablero),
    % Hereda alfa a sus descendientes
    alfa(Tablero,A),
    modificaAlfa(NuevoTablero,A),
    % Restricciones para el valor del descendiente
    ((Jugador = 1, ValorNegativo #= Aptitud * 1) ;
    (Jugador = 2, ValorNegativo #= Aptitud * -1)),
    ValorNegativo #> A,
    length(Jugada,TamañoJugada),
    siguiente_turno(Casilla,TamañoJugada,Jugador,JugadorSiguiente),
    ProfundidadSiguiente #= Profundidad+1,
    valor_negamax(NuevoTablero,JugadorSiguiente,ProfundidadSiguiente,Aptitud),
    modificaAlfa(Tablero,ValorNegativo).

% negamax/4
% negamax(+Tablero,+Jugador,+Profundidad,-Aptitud)
% Aptitud es la diferencia entre los puntos del jugador 1 y el jugador 2, se
% calcula con el algoritmo negamax con podas Alpha-Beta.
negamax(Tablero,_,Profundidad,Aptitud) :-
    (fin(Tablero) ; horizonte(Profundidad)),
    aptitud(Tablero,Aptitud),!.
negamax(Tablero,Jugador,Profundidad,Aptitud) :-
    generar_jugada(Casilla,Tablero,Jugada),
    jugada(Jugador,Casilla,Jugada,Tablero,NuevoTablero),
    length(Jugada,TamañoJugada),
    siguiente_turno(Casilla,TamañoJugada,Jugador,JugadorSiguiente),
    ProfundidadSiguiente #= Profundidad+1,
    negamax(NuevoTablero,JugadorSiguiente,ProfundidadSiguiente,Aptitud).

% generar_jugada/3
% generar_jugada(+Casilla,+Tablero,-Jugada)
% Casilla y Jugada son la casilla y jugada que se deben realizar en el tablero
% para obtener la mejor aptitud posible.
generar_jugada(Casilla,Tablero,Jugada) :-
    Casilla in 0..6,
    Jugada = [_|_],
    nth1(Casilla,Tablero,[Amarillas,Verdes,Rojas]),
    length(FichasAmarillas,Amarillas), maplist(=(a),FichasAmarillas),
    length(FichasVerdes,Verdes), maplist(=(v),FichasVerdes),
    length(FichasRojas,Rojas), maplist(=(r),FichasRojas),
    append([FichasAmarillas,FichasVerdes,FichasRojas],Fichas),
    permutation(Fichas,Jugada).

% tirar/3
% tirar(+Tablero,-Casilla,-Jugada)
% Casilla y Jugada son la casilla y jugada que se deben realizar en el tablero
% para obtener la mejor aptitud posible.
tirar(Tablero,CT,JT) :-
    horizonte(H), Tiros #= 10 * H,
    findnsols(Tiros,C-J,generar_jugada(C,Tablero,J),Lista),
    random_member(C1-J1,Lista),
    jugada(1,C1,J1,Tablero,T1),
    random_member(C2-J2,Lista),
    jugada(1,C2,J2,Tablero,T2),
    random_member(C3-J3,Lista),
    jugada(1,C3,J3,Tablero,T3),
    random_member(C4-J4,Lista),
    jugada(1,C4,J4,Tablero,T4),
    random_member(C5-J5,Lista),
    jugada(1,C5,J5,Tablero,T5),
    maplist(aptitud,[T1,T2,T3,T4,T5],A),
    max_member(Max,A),
    nth1(P,A,Max),
    nth1(P,[J1,J2,J3,J4,J5],JT),
    nth1(P,[C1,C2,C3,C4,C5],CT).

% aptitud/2
% aptitud(+Tablero,-Aptitud)
% Aptitud es la diferencia entre los puntos del jugador 1 y el jugador 2
% en el tablero dado.
aptitud(Tablero,Aptitud) :-
    Tablero = [C1,C2,C3,C4,C5,C6, BaseJ1, C7,C8,C9,C10,C11,C12, BaseJ2],
    J1 = [BaseJ1,C7,C8,C9,C10,C11,C12],
    maplist(puntaje_casilla,J1,PuntajesJ1),
    sum_list(PuntajesJ1,PuntajeJ1),
    J2 = [BaseJ2,C1,C2,C3,C4,C5,C6],
    maplist(puntaje_casilla,J2,PuntajesJ2),
    sum_list(PuntajesJ2,PuntajeJ2),
    Aptitud #= PuntajeJ1 - PuntajeJ2.

% color_jugador/2
% color_jugador(+Jugador,-Color)
% Color es el color del jugador indicado.
color_jugador(1,blue).
color_jugador(2,magenta).

% ficha/1
% ficha(-Ficha)
% Ficha es el caracter que representa una ficha.
ficha('⬤').

% imprime_marco/1
% imprime_marco(+Tipo)
% Imprime un marco del tablero, puede ser superior o inferior.
imprime_marco(superior) :-
    format('     Jugador 1       6           5           4           3           2           1       Jugador 2'), nl.
imprime_marco(inferior) :-
    format('     Jugador 1       1           2           3           4           5           6       Jugador 2'), nl.

% imprime_sangría/0
% Imprime una sangría para centrar el tablero.
imprime_sangría() :-
    format('    ').

% imprime_espacios_en_blanco/1
% imprime_espacios_en_blanco(+Cantidad)
% Imprime una cantidad de espacios en blanco.
imprime_espacios_en_blanco(0).
imprime_espacios_en_blanco(Cantidad) :-
    Cantidad #> 0,
    format('  '),
    NuevaCantidad #= Cantidad - 1,
    imprime_espacios_en_blanco(NuevaCantidad).

% imprime_borde/2
% imprime_borde(+Jugador,+Texto)
% Imprime un borde de una casilla.
imprime_borde(Jugador,Texto) :-
    color_jugador(Jugador,ColorTexto),
    ansi_format([bold,fg(ColorTexto)],Texto,[]).
imprime_borde(centro,Jugador) :-
    imprime_borde(Jugador,' ---------   ---------   ---------   ---------   ---------   ---------  ').
imprime_borde(superior) :-
    imprime_sangría(),
    imprime_borde(1,' ---------   ---------   ---------   ---------   ---------   ---------   ---------'),
    imprime_borde(2,'   ---------'), nl.
imprime_borde(inferior) :-
    imprime_sangría(),
    imprime_borde(1,' ---------'),
    imprime_borde(2,'   ---------   ---------   ---------   ---------   ---------   ---------   ---------'), nl.

% imprime_fichas/2
% imprime_fichas(+Color,+Cantidad)
% Imprime una cantidad de fichas de un color.
imprime_fichas(_,0).
imprime_fichas(Color,Cantidad) :-
    Cantidad #> 0,
    ficha(Ficha),
    ansi_format([fg(Color)],"~w ",[Ficha]),
    NuevaCantidad #= Cantidad - 1,
    imprime_fichas(Color,NuevaCantidad).

% imprime_fichas/1
% imprime_fichas(+Fichas)
% Imprime una lista de fichas.
imprime_fichas([]).
imprime_fichas([a|Resto]) :-
    ficha(Ficha),
    ansi_format([fg(yellow)],"~w ",[Ficha]),
    imprime_fichas(Resto).
imprime_fichas([v|Resto]) :-
    ficha(Ficha),
    ansi_format([fg(green)],"~w ",[Ficha]),
    imprime_fichas(Resto).
imprime_fichas([r|Resto]) :-
    ficha(Ficha),
    ansi_format([fg(red)],"~w ",[Ficha]),
    imprime_fichas(Resto).

% imprime_renglón_casilla/3
% imprime_renglón_casilla(+Jugador,+Fichas,-Resto)
% Imprime un renglón de una casilla y devuelve el resto de las fichas.
imprime_renglón_casilla(Jugador,Fichas,[0,0,0]) :-
    Fichas = [Amarillas,Verdes,Rojas],
    Amarillas + Verdes + Rojas #=< 4, % 4 es el ancho de las casillas, por lo que no puede haber más de esa cantidad en un solo renglón
    imprime_borde(Jugador,'| '),
    once(imprime_fichas(yellow,Amarillas)),
    once(imprime_fichas(green,Verdes)),
    once(imprime_fichas(red,Rojas)),
    Espacios #= 4 - (Amarillas + Verdes + Rojas),
    once(imprime_espacios_en_blanco(Espacios)),
    imprime_borde(Jugador,'| ').
imprime_renglón_casilla(Jugador,Fichas,Resto) :-
    Fichas = [Amarillas,Verdes,Rojas],
    Fichas ins 0..12,
    Amarillas + Verdes + Rojas #> 4,
    Renglón = [NuevasAmarillas,NuevasVerdes,NuevasRojas],
    Renglón ins 0..4,
    NuevasAmarillas + NuevasVerdes + NuevasRojas #= 4,
    Resto = [RestoAmarillas,RestoVerdes,RestoRojas],
    Resto ins 0..12,
    Amarillas #= NuevasAmarillas + RestoAmarillas,
    Verdes #= NuevasVerdes + RestoVerdes,
    Rojas #= NuevasRojas + RestoRojas,
    imprime_borde(Jugador,'| '),
    once(imprime_fichas(yellow,NuevasAmarillas)),
    once(imprime_fichas(green,NuevasVerdes)),
    once(imprime_fichas(red,NuevasRojas)),
    imprime_borde(Jugador,'| ').

% imprime_renglón_tablero/3
% imprime_renglón_tablero(+Jugador,+Tablero,-Resto)
% Imprime un renglón del tablero y devuelve el resto del tablero.
imprime_renglón_tablero(1,Tablero,Resto) :-
    Tablero =   [C1,C2,C3,C4,C5,C6,BaseJ1,
                C7,C8,C9,C10,C11,C12,BaseJ2],
    imprime_sangría(),
    imprime_renglón_casilla(1,BaseJ1,RestoBaseJ1),
    imprime_renglón_casilla(1,C6,RC6),
    imprime_renglón_casilla(1,C5,RC5),
    imprime_renglón_casilla(1,C4,RC4),
    imprime_renglón_casilla(1,C3,RC3),
    imprime_renglón_casilla(1,C2,RC2),
    imprime_renglón_casilla(1,C1,RC1),
    imprime_renglón_casilla(2,BaseJ2,RestoBaseJ2),

    Resto = [RC1,RC2,RC3,RC4,RC5,RC6,RestoBaseJ1,
            C7,C8,C9,C10,C11,C12,RestoBaseJ2],
    nl.
imprime_renglón_tablero(2,Tablero,Resto) :-
    Tablero =   [C1,C2,C3,C4,C5,C6,BaseJ1,
                C7,C8,C9,C10,C11,C12,BaseJ2],
    imprime_sangría(),
    imprime_renglón_casilla(1,BaseJ1,RestoBaseJ1),
    imprime_renglón_casilla(2,C7,RC7),
    imprime_renglón_casilla(2,C8,RC8),
    imprime_renglón_casilla(2,C9,RC9),
    imprime_renglón_casilla(2,C10,RC10),
    imprime_renglón_casilla(2,C11,RC11),
    imprime_renglón_casilla(2,C12,RC12),
    imprime_renglón_casilla(2,BaseJ2,RestoBaseJ2),
    Resto = [C1,C2,C3,C4,C5,C6,RestoBaseJ1,
            RC7,RC8,RC9,RC10,RC11,RC12,RestoBaseJ2],
    nl.

% imprime_resultado/2
% imprime_resultado(+PuntajeJ1,+PuntajeJ2)
% Imprime el resultado del juego en la consola.
imprime_resultado(Puntaje,Puntaje) :-
    ansi_format([bold,fg(green)],'~n~t ¡Fin del juego!   ~60|~n~n               ',[]),
    ansi_format([bold,bg(blue)],'       Empate      ',[]),
    format('            Jugador 1              ~w puntos~n               ',[Puntaje]),
    ansi_format([bold,bg(magenta)],'       Empate      ',[]),
    format('            Jugador 2              ~w puntos',[Puntaje]).
imprime_resultado(PuntajeJ1,PuntajeJ2) :-
    (((PuntajeJ1 > PuntajeJ2)
    -> (Ganador = 1, Otro = 2,
        PuntosGanador = PuntajeJ1, PuntosOtro = PuntajeJ2,
        Color = blue));
    ((PuntajeJ2 > PuntajeJ1)
    -> (Ganador = 2, Otro = 1,
        PuntosGanador = PuntajeJ2, PuntosOtro = PuntajeJ1,
        Color = magenta))),
    ansi_format([bold,fg(green)],'~n~t ¡Fin del juego!   ~60|~n~n               ',[]),
    ansi_format([bold,bg(Color)],'      Ganador      ',[]),
    format('            Jugador ~w              ~w puntos~n                                  ',[Ganador,PuntosGanador]),
    format('            Jugador ~w              ~w puntos',[Otro,PuntosOtro]).

% imprime_jugada/2
% imprime_jugada(+Casilla,+Jugada)
% Imprime la casilla y la jugada en la consola.
imprime_jugada(Casilla,Jugada) :-
    Casilla in 1..6,
    format('~n'),
    imprime_sangría(),
    ansi_format([fg(cyan),bold],'# de casilla: ',[]),
    ansi_format([bold],'~w ',[Casilla]),
    length(Jugada,Cantidad),
    CasillaSiguiente #= Casilla + 1,
    imprime_casilla(CasillaSiguiente,Cantidad),
    imprime_sangría(),
    ansi_format([fg(green)],'Ficha:         ',[]),
    imprime_fichas(Jugada),
    format('~n').

% imprime_casilla/2
% imprime_casilla(+Casilla,+Cantidad)
% Imprime la casilla con el número de fichas indicado.
imprime_casilla(_,0) :-
    format('~n').
imprime_casilla(Casilla,Cantidad) :-
    Cantidad > 0,
    ((Casilla > 0, Casilla =< 6, Imprime = Casilla) ;
    (Casilla > 13, Casilla =< 19, Imprime #= Casilla - 13)),
    ansi_format([fg(blue),bold],'~w ',[Imprime]),
    SiguienteCantidad #= Cantidad - 1,
    SiguienteCasilla #= Casilla + 1,
    imprime_casilla(SiguienteCasilla,SiguienteCantidad).
imprime_casilla(Casilla,Cantidad) :-
    Cantidad > 0,
    (Casilla = 7 ; Casilla = 20),
    ansi_format([bold],'~w ',['B']),
    SiguienteCantidad #= Cantidad - 1,
    SiguienteCasilla #= Casilla + 1,
    imprime_casilla(SiguienteCasilla,SiguienteCantidad).
imprime_casilla(Casilla,Cantidad) :-
    Cantidad > 0,
    ((Casilla > 7, Casilla =< 13, Imprime #= Casilla - 7) ;
    (Casilla > 20, Casilla =< 26, Imprime #= Casilla - 20)),
    ansi_format([fg(magenta),bold],'~w ',[Imprime]),
    SiguienteCantidad #= Cantidad - 1,
    SiguienteCasilla #= Casilla + 1,
    imprime_casilla(SiguienteCasilla,SiguienteCantidad).

% imprime_tablero/1
% imprime_tablero(+Tablero)
% Imprime el tablero en la consola.
imprime_tablero(Tablero) :-
    nl,
    imprime_marco(superior),
    imprime_borde(superior),
    imprime_renglón_tablero(1,Tablero,Resto1),
    imprime_renglón_tablero(1,Resto1,Resto2),
    imprime_renglón_tablero(1,Resto2,Resto3),
    imprime_renglón_tablero(1,Resto3,Resto4),
    nth1(7,Resto4,BaseJ1,Contexto1),
    nth1(7,Resto5,R3,Contexto1),
    nth1(14,Resto5,BaseJ2,Contexto2),
    nth1(14,RestoX,R4,Contexto2),
    imprime_sangría(),imprime_renglón_casilla(1,BaseJ1,R1),imprime_borde(centro,1),imprime_renglón_casilla(2,BaseJ2,R2), nl,
    imprime_sangría(),imprime_renglón_casilla(1,R1,R3),imprime_borde(centro,2),imprime_renglón_casilla(2,R2,R4), nl,
    imprime_renglón_tablero(2,RestoX,Resto6),
    imprime_renglón_tablero(2,Resto6,Resto7),
    imprime_renglón_tablero(2,Resto7,Resto8),
    imprime_renglón_tablero(2,Resto8,_),
    imprime_borde(inferior),
    imprime_marco(inferior),
    nl.
