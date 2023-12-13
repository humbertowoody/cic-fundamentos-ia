% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Tarea 6 - Agente Jugador de Gato 4x4
%           Implementación de un agente jugador de Gato 4x4 usando el algoritmo
%           Minimax con podas Alpha-Beta.
%
% Propuesta de Ejecución:
%  ?- inicia_juego(1-2).  % Fácil, respuesta rápida, posible de vencer.
%  ?- inicia_juego(3-5).  % Medio/Difícil, respuesta un poco lenta pero aceptable,
%                         % empate garantizado.
%  ?- inicia_juego(6-10). % Difícil, respuesta lenta, empate garantizado.
%
% Fundamentos de Inteligencia Artificial, CIC, IPN, 2023.
% =============================================================================

% Librería CLPFD.
:- use_module(library(clpfd)).

% símbolo/2
% símbolo(+Identificador, -Símbolo)
% Símbolo es el símbolo asociado al jugador Identificador.
símbolo(1, x).
símbolo(2, o).

% símboloVacío/1
% símboloVacío(-Símbolo)
% Símbolo representa una casilla vacía en el tablero.
símboloVacío(-).

% estadoActual/2
% estadoActual(-Tablero, -Jugador)
% Tablero es el tablero de juego actual y Jugador es el jugador que tiene el turno.
:- dynamic estadoActual/2.

% horizonte/1
% horizonte(-Horizonte)
% Horizonte es la profundidad máxima de búsqueda del algoritmo Minimax.
:- dynamic horizonte/1.

% tableroVacío/1
% tableroVacío(-Tablero)
% Tablero es un tablero de juego vacío.
tableroVacío([R, R, R, R]) :- símboloVacío(S), R = [S, S, S, S].

% imprimirTablero/1
% imprimirTablero(+Tablero)
% Imprime el estado actual de Tablero.
imprimirTablero([A, B, C, D]) :-
    format('~n           1     2     3     4~n~n'),
    maplist(imprimirRenglón, [a-A, b-B, c-C, d-D]).
imprimirRenglón(Letra-Renglón) :-
    format('    ~w   |  ~w  |  ~w  |  ~w  |  ~w  |~n', [Letra|Renglón]),
    format('        +-----+-----+-----+-----+~n').

% siguiente_turno/2
% siguiente_turno(+JugadorActual, -SiguienteJugador)
% SiguienteJugador es el jugador que sigue después de JugadorActual.
siguiente_turno(1, 2).
siguiente_turno(2, 1).

% jugada/2
% jugada((+Tablero, +Jugador), (-NuevoTablero, -NuevoJugador))
% Genera NuevoTablero y NuevoJugador a partir de una jugada válida.
jugada( (Tablero, Jugador), (NuevoTablero, NuevoJugador) ) :-
    append(Arriba, [Renglón|Abajo], Tablero),
    append(Izquierda, [ - |Derecha], Renglón),
    símbolo(Jugador, Símbolo),
    append(Izquierda, [Símbolo|Derecha], NuevoRenglón),
    append(Arriba, [NuevoRenglón|Abajo], NuevoTablero),
    siguiente_turno(Jugador, NuevoJugador).

% victoria/2
% victoria(+Tablero, -Ganador)
% Determina si hay un ganador en el tablero actual.
victoria(Tablero, Ganador) :-
    (   victoria_horizontal(Tablero, Ganador)
    ;   victoria_vertical(Tablero, Ganador)
    ;   victoria_diagonal(Tablero, Ganador)
    ).

% victoria_horizontal/2
% victoria_horizontal(+Tablero, -Ganador)
% Verifica si hay una victoria horizontal en el tablero.
victoria_horizontal(Tablero, Ganador) :-
    member(Fila, Tablero),
    todos_iguales(Fila, Símbolo),
    símbolo(Ganador, Símbolo).
    %format('Victoria horizontal para jugador ~w con símbolo ~w en fila ~w~n', [Ganador, Símbolo, Fila]).

% victoria_vertical/2
% victoria_vertical(+Tablero, -Ganador)
% Verifica si hay una victoria vertical en el tablero.
victoria_vertical(Tablero, Ganador) :-
    transpose(Tablero, TableroTranspuesto),
    member(Columna, TableroTranspuesto),
    todos_iguales(Columna, Símbolo),
    símbolo(Ganador, Símbolo).
    %format('Victoria vertical para jugador ~w con símbolo ~w en columna ~w~n', [Ganador, Símbolo, Columna]).

% victoria_diagonal/2
% victoria_diagonal(+Tablero, -Ganador)
% Verifica si hay una victoria en alguna de las diagonales del tablero.
victoria_diagonal(Tablero, Ganador) :-
    (
        % Verificación de la diagonal principal (de arriba izquierda a abajo derecha)
        Tablero = [[A, _, _, _],
                   [_, B, _, _],
                   [_, _, C, _],
                   [_, _, _, D]],
        A \= -, A = B, B = C, C = D,
        símbolo(Ganador, A)
        %format('Victoria diagonal principal para jugador ~w con símbolo ~w.~n', [Ganador, A])
    ;
        % Verificación de la diagonal secundaria (de arriba derecha a abajo izquierda)
        Tablero = [[_, _, _, A],
                   [_, _, B, _],
                   [_, C, _, _],
                   [D, _, _, _]],
        A \= -, A = B, B = C, C = D,
        símbolo(Ganador, A)
        %format('Victoria diagonal secundaria para jugador ~w con símbolo ~w.~n', [Ganador, A])
    ).

% todos_iguales/2
% todos_iguales(+Lista, -Elemento)
% Verifica si todos los elementos de la lista son iguales y no vacíos.
todos_iguales([H|T], H) :-
    H \= -,
    forall(member(X, T), X = H).

% empate/1
% empate(+Tablero)
% Es verdadero si Tablero representa un estado de empate.
empate(T) :-
    símboloVacío(V),
    forall(member(R, T), \+ member(V, R)).

% inicia_juego/1
% inicia_juego(+Horizonte)
% Inicia el juego con un horizonte de profundidad especificado.
inicia_juego(Horizonte) :-
    Horizonte >= 1, % Validar horizonte
    símbolo(1, S1),
    símbolo(2, S2),
    format('Iniciando juego con horizonte ~w.~n', [Horizonte]),
    format('El jugador 1 (humano) usa el símbolo: ~w.~n', [S1]),
    format('El jugador 2 (agente) usa el símbolo: ~w.~n', [S2]),
    format('~nLas coordenadas se ingresan como (renglón, columna).~nEjemplo:~n    (c,3).~n~n'),
    retractall(horizonte(_)),
    assert(horizonte(Horizonte)),
    tableroVacío(TableroVacío),
    retractall(estadoActual(_, _)),
    assert(estadoActual(TableroVacío, 1)),
    jugar.

% jugar/0
% jugar
% Controla el flujo del juego entre el jugador humano y el agente.
jugar :-
    estadoActual(Tablero, Jugador),
    símbolo(Jugador, Símbolo),
    format('Turno del jugador ~w (~w).~n', [Jugador, Símbolo]),
    ( victoria(Tablero, _) -> anunciar_ganador(Tablero)
    ; empate(Tablero)      -> anunciar_empate(Tablero)
    ; Jugador = 1         -> turno_humano
    ; Jugador = 2         -> turno_agente ).

% turno_humano/0
% turno_humano
% Maneja el turno del jugador humano.
turno_humano :-
    estadoActual(Tablero, _),
    imprimirTablero(Tablero),
    pedir_jugada(Renglón, Columna),
    tiro(Renglón, Columna, NuevoTablero),
    retractall(estadoActual(_, _)),
    assert(estadoActual(NuevoTablero, 2)),
    jugar.

% pedir_jugada/2
% pedir_jugada(-Renglón, -Columna)
% Pide al usuario que ingrese su jugada.
pedir_jugada(Renglón, Columna) :-
    format('Ingresa tu jugada (renglón, columna): '),
    read((Renglón, Columna)),
    % Validar jugada
    renglón(Renglón, RenglónNúmero),
    between(1, 4, Columna),
    estadoActual(Tablero, _),
    símboloVacío(V),
    nth1(RenglónNúmero, Tablero, Fila),
    nth1(Columna, Fila, V).

% turno_agente/0
% turno_agente
% Maneja el turno del agente.
turno_agente :-
    estadoActual(Tablero, Jugador),
    horizonte(Horizonte),
    %format('Tablero actual:~n'),
    %imprimirTablero(Tablero),
    %format('Calculando jugada con horizonte ~w...~n', [Horizonte]),
    minimax(Tablero, Jugador, Horizonte, -inf, inf, NuevoTablero, _),
    %format('Jugada calculada.~n'),
    %format('NuevoTablero: ~w~n', [NuevoTablero]),
    %format('Valor: ~w~n', [Valor]),
    %imprimirTablero(NuevoTablero),
    retractall(estadoActual(_, _)),
    assert(estadoActual(NuevoTablero, 1)),
    jugar.

% anunciar_ganador/1
% anunciar_ganador(+Tablero)
% Anuncia el ganador del juego.
anunciar_ganador(Tablero) :-
    imprimirTablero(Tablero),
    ( victoria(Tablero, Jugador) -> format('Jugador ~w gana el juego.~n', [Jugador])
    ; format('No hay un ganador claro.~n') ).

% anunciar_empate/1
% anunciar_empate(+Tablero)
% Anuncia un empate.
anunciar_empate(Tablero) :-
    imprimirTablero(Tablero),
    format('El juego termina en empate.~n').

% valor/3
% valor(+Tablero, +Jugador, -Valor)
% Valor es la evaluación heurística del Tablero para Jugador.
valor(Tablero, Jugador, Valor) :-
    ( victoria(Tablero, Jugador) ->
        Valor = 1000  % Victoria para el jugador
    ; oponente(Jugador, Oponente), victoria(Tablero, Oponente) ->
        Valor = -1000 % Derrota para el jugador
    ; empate(Tablero) ->
        Valor = 0     % Empate
    ; Valor = 0     % Si no hay victoria, derrota ni empate, se considera empate por defecto
    ).

% minimax/7
% minimax(+Tablero, +Jugador, +Profundidad, +Alpha, +Beta, -MejorMovimiento, -Valor)
% Determina el MejorMovimiento y su Valor usando el algoritmo Minimax con podas Alpha-Beta.
minimax(Tablero, Jugador, Profundidad, Alpha, Beta, MejorMovimiento, Valor) :-
    %format('Minimax llamado con Profundidad: ~w, Jugador: ~w~n', [Profundidad, Jugador]),
    ( victoria(Tablero, Jugador) ->
        Valor = 1000, MejorMovimiento = Tablero
        %format('Victoria detectada para jugador ~w, Valor: ~w~n', [Jugador, Valor])
    ; empate(Tablero) ->
        Valor = 0, MejorMovimiento = Tablero
        %format('Empate detectado, Valor: 0~n')
    ; Profundidad = 0 ->
        valor(Tablero, Jugador, Valor), MejorMovimiento = Tablero
        %format('Profundidad 0 alcanzada, Valor heurístico: ~w~n', [Valor])
    ; siguiente_turno(Jugador, OtroJugador),
      findall(Movimiento, jugada((Tablero, Jugador), (Movimiento, _)), Movimientos),
      alpha_beta(Movimientos, OtroJugador, Profundidad, Alpha, Beta, Tablero, (MejorMovimiento, -inf), MejorMovimiento, Valor)
      %format('Mejor movimiento: ~w con valor: ~w~n', [MejorMovimiento, Valor])
    ).

% alpha_beta/9
% alpha_beta(+Movimientos, +Jugador, +Profundidad, +Alpha, +Beta, +MejorAnterior, +ValorAnterior, -MejorMovimiento, -Valor)
% Función auxiliar para Minimax con podas Alpha-Beta.
alpha_beta([], _, _, _, _, _, (E, Valor), E, Valor) :- !.
    %format('No hay más movimientos, Valor retornado: ~w~n', [Valor]), !.
alpha_beta([Movimiento|Movimientos], Jugador, Profundidad, Alpha, Beta, MejorAnterior, (MejorAntVal, MejorAntScore), MejorMovimiento, Valor) :-
    %format('Alpha-Beta evaluando movimiento: ~w~n', [Movimiento]),
    NuevaProfundidad is Profundidad - 1,
    minimax(Movimiento, Jugador, NuevaProfundidad, -Beta, -Alpha, _, MovimientoValor),
    %format('Valor devuelto por minimax: ~w para movimiento: ~w~n', [MovimientoValor, Movimiento]),
    MovimientoValorNegado is -MovimientoValor,
    (MovimientoValorNegado > Alpha ->
        NuevoAlpha = MovimientoValorNegado,
        NuevoMejorMovimiento = Movimiento,
        %format('Nuevo alpha: ~w, Nuevo mejor movimiento: ~w~n', [NuevoAlpha, NuevoMejorMovimiento]),
        (NuevoAlpha >= Beta ->
            MejorMovimiento = NuevoMejorMovimiento, Valor = NuevoAlpha
            %format('Poda Beta, Mejor movimiento: ~w, Valor: ~w~n', [MejorMovimiento, Valor])
        ;   alpha_beta(Movimientos, Jugador, Profundidad, NuevoAlpha, Beta, NuevoMejorMovimiento, (NuevoMejorMovimiento, NuevoAlpha), MejorMovimiento, Valor)
        )
    ;   alpha_beta(Movimientos, Jugador, Profundidad, Alpha, Beta, MejorAnterior, (MejorAntVal, MejorAntScore), MejorMovimiento, Valor)
    ).

% tiro/3
% tiro(+Renglón, +Columna, -NuevoTablero)
% Realiza un tiro en el Tablero en la posición indicada.
tiro(RenglónLetra, Columna, NuevoTablero) :-
    estadoActual(Tablero, Jugador),
    símbolo(Jugador, Símbolo),
    renglón(RenglónLetra, Renglón),
    nth1(Renglón, Tablero, Fila, RestoTablero),
    nth1(Columna, Fila, _, RestoFila),
    nth1(Columna, NuevaFila, Símbolo, RestoFila),
    nth1(Renglón, NuevoTablero, NuevaFila, RestoTablero),
    siguiente_turno(Jugador, NuevoJugador),
    retractall(estadoActual(_,_)),
    assert(estadoActual(NuevoTablero, NuevoJugador)).

% oponente/2
% oponente(+Jugador, -Oponente)
% Oponente es el oponente de Jugador.
oponente(1, 2).
oponente(2, 1).

% renglón/2
% renglón(+Letra, -Renglón)
% Renglón es el número de renglón asociado a la letra Letra.
renglón(a, 1).
renglón(b, 2).
renglón(c, 3).
renglón(d, 4).

