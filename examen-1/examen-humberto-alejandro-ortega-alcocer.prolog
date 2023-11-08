% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Examen - Metro de la Ciudad de México.
%
% Indicaciones:
%   - Usar el conocimiento entregado sobre la red Metro de la Ciudad de México.
%   - Programar los siguientes predicados:
%     1. mejor_ruta(<origen>,<destino>, <mejor-ruta>, <tiempo>).
%     2. reporte_tiempo(<origen>, <destino>).
%     3. instrucciones(<origen>, <destino>).
%
% Propuesta de ejecución:
%   1. mejor_ruta(cuatro_caminos, politécnico, Ruta, Tiempo).
%   2. reporte_tiempo(cuatro_caminos, politécnico).
%   3. instrucciones(cuatro_caminos, politécnico).
%
% Fundamentos de Inteligencia Artificial, CIC, IPN, 2023.
% =============================================================================

% =============================================================================
% Carga de la base de conocimiento con el grafo del Metro.
% =============================================================================
:- consult('metro-CDMX.prolog').

% =============================================================================
% Se incluye la librería clpfd para poder usar restricciones.
% =============================================================================
:- use_module(library(clpfd)).

% =============================================================================
% Hechos
% =============================================================================

% máximo_número_de_rutas/1
% máximo_número_de_rutas(-Número)
% Número es el máximo número de rutas que se pueden calcular, esto debido a que
% se desbordará la pila de Prolog si tratamos de calcular todas las rutas posibles.
máximo_número_de_rutas(2000).

% texto_número_línea/2
% texto_número_línea(+Línea,-Texto)
% Texto es el número (símbolo) de la línea del metro (para simplificar operaciones)
texto_número_línea(línea_1,'1').
texto_número_línea(línea_2,'2').
texto_número_línea(línea_3,'3').
texto_número_línea(línea_4,'4').
texto_número_línea(línea_5,'5').
texto_número_línea(línea_6,'6').
texto_número_línea(línea_7,'7').
texto_número_línea(línea_8,'8').
texto_número_línea(línea_9,'9').
texto_número_línea(línea_A,'A').
texto_número_línea(línea_B,'B').
texto_número_línea(línea_12,'12').

% =============================================================================
% Predicados
% =============================================================================

% ruta_h/4
% ruta_h(+EstaciónOrigen,+EstaciónDestino,+EstacionesVisitadas,-Ruta)
% Ruta es una lista de estaciones que se deben seguir para llegar de EstaciónOrigen a
% EstaciónDestino sin pasar por las estaciones de EstacionesVisitadas.
ruta_h(Estación,Estación,_,[Estación]).
ruta_h(EstaciónOrigen,EstaciónDestino,_,[EstaciónOrigen,EstaciónDestino]):-
    EstaciónOrigen \== EstaciónDestino,
    (sigue(EstaciónOrigen,EstaciónDestino,_) ; sigue(EstaciónDestino,EstaciónOrigen,_)).
ruta_h(EstaciónOrigen,EstaciónDestino,EstacionesVisitadas,[EstaciónOrigen|Ruta]):-
    EstaciónOrigen \== EstaciónDestino,
    \+ sigue(EstaciónOrigen,EstaciónDestino,_),
    \+ sigue(EstaciónDestino,EstaciónOrigen,_),
    (sigue(EstaciónOrigen,X,_);
    sigue(X,EstaciónOrigen,_)),
    \+ member(X,EstacionesVisitadas),
    ruta_h(X,EstaciónDestino,[X|EstacionesVisitadas],Ruta).

% ruta/3
% ruta(+EstaciónOrigen,+EstaciónDestino,-Ruta)
% Ruta es una lista de estaciones que se deben seguir para llegar de EstaciónOrigen a
% EstaciónDestino.
ruta(EstaciónOrigen,EstaciónDestino,Ruta):-
    ruta_h(EstaciónOrigen,EstaciónDestino,[EstaciónOrigen],Ruta).

% Encuentra todas las rutas posibles entre dos estaciones
todas_rutas(EstaciónOrigen, EstaciónDestino, TodasLasRutas):-
    findall(Ruta, ruta(EstaciónOrigen, EstaciónDestino, Ruta), TodasLasRutas).

% grado_estación/2
% grado_estación(+Estación,-Grado)
% Grado es la cantidad de estaciones adyacentes a Estación.
grado_estación(Estación,Grado):-
    findall(Estación_Contigua,sigue(Estación,Estación_Contigua,_),A),
    findall(Estación_Contigua,sigue(Estación_Contigua,Estación,_),B),
    length(A,LA), length(B,LB),
    Grado #= LA + LB.

% misma_línea/2
% misma_línea(+EstaciónOrigen,+EstaciónDestino)
% Verifica que las dos estaciones proporcionadas pertenezcan a la misma línea del metro.
misma_línea(EstaciónOrigen,EstaciónDestino):-
    (sigue(EstaciónOrigen,_,Línea);
    sigue(_,EstaciónOrigen,Línea)),
    (sigue(EstaciónDestino,_,Línea);
    sigue(_,EstaciónDestino,Línea)),!.

% tiempo_arista/3
% tiempo_arista(+Ruta,+EstaciónDestino,-TiempoArista-Transborde)
% Calcula el tiempo que tomará el trayecto entre dos estaciones contiguas de una misma ruta.
tiempo_arista([_],_,0-no).
tiempo_arista([EstaciónDestino|_],EstaciónDestino,TiempoArista-no):-
    valor_parámetro(tiempo_tramo,TiempoTramo),
    grado_estación(EstaciónDestino,Grado),
    TiempoArista #= TiempoTramo * Grado.
tiempo_arista(Ruta,EstaciónDestino,TiempoArista-no):-
    append(_,[EstaciónDestino],Ruta),
    valor_parámetro(tiempo_tramo,TiempoTramo),
    grado_estación(EstaciónDestino,Grado),
    TiempoArista #= TiempoTramo * Grado.
tiempo_arista(Ruta,EstaciónDestino,TiempoArista-no):-
    append(_,[EstaciónPrevia,EstaciónDestino,EstaciónSiguiente|_],Ruta),
    misma_línea(EstaciónPrevia,EstaciónSiguiente),
    valor_parámetro(tiempo_tramo,TiempoTramo),
    grado_estación(EstaciónDestino,Grado),
    TiempoArista #= TiempoTramo * Grado.
tiempo_arista(Ruta,EstaciónDestino,TiempoArista-Transborde):-
    append(_,[EstaciónPrevia,EstaciónDestino,EstaciónSiguiente|_],Ruta),
    \+ misma_línea(EstaciónPrevia,EstaciónSiguiente),
    valor_parámetro(tiempo_transbordo,TiempoTransborde),
    valor_parámetro(tiempo_tramo,TiempoTramo),
    grado_estación(EstaciónDestino,Grado),
    TiempoArista #= TiempoTransborde + (TiempoTramo * Grado),
    (sigue(EstaciónSiguiente,EstaciónDestino,Transborde);
    sigue(EstaciónDestino,EstaciónSiguiente,Transborde)).

% tiempo_ruta/3
% tiempo_ruta(+Ruta,-TiempoAristas,-TiempoTotal)
% TiempoAristas es una lista de pares ordenados que contiene el tiempo que toma cada tramo de
% dos estaciones y si es que existe algún transborde. TiempoTotal es un número entero
% conformado por la suma de los valores de los tramos y los tiempos inicial y final.
tiempo_ruta([_],[0-no],0).
tiempo_ruta(Ruta,TiempoAristas,TiempoTotal):-
    Ruta = [_|Aristas],
    maplist(tiempo_arista(Ruta),Aristas,TiempoAristas),
    suma_tiempos(TiempoAristas,TiempoTramos),
    valor_parámetro(tiempo_inicial,TiempoInicial),
    valor_parámetro(tiempo_final,TiempoFinal),
    TiempoTotal #= TiempoTramos + TiempoInicial + TiempoFinal.

% suma_tiempos/2
% suma_tiempos(+TiempoAristas,-TiempoTotal)
% TiempoAristas es una lista de pares ordenados que contiene el tiempo que toma cada tramo de
% dos estaciones y si es que existe algún transborde. TiempoTotal es un número entero
% conformado por la suma de los valores de los tramos.
suma_tiempos([TiempoActual-_],TiempoTotal):-
    TiempoTotal = TiempoActual.
suma_tiempos([TiempoActual-_|TiempoAristas],TiempoTotal):-
    TiempoTotal #= TiempoAnterior + TiempoActual,
    suma_tiempos(TiempoAristas,TiempoAnterior).

% ruta_corta/4
% ruta_corta(+Rutas,-MejorRuta,-ListaTramos,-TiempoMínimo)
% Rutas es una lista de listas de estaciones que se deben seguir para llegar de EstaciónOrigen a
% EstaciónDestino. MejorRuta es la ruta con el menor tiempo de viaje. ListaTramos es una lista
% de pares ordenados que contiene el tiempo que toma cada tramo de dos estaciones y si es que
% existe algún transborde. TiempoMínimo es un número entero conformado por el menor tiempo de
% viaje de todas las rutas.
ruta_corta([MejorRuta], MejorRuta,ListaTramos,TiempoMínimo):-
    tiempo_ruta(MejorRuta,ListaTramos,TiempoMínimo).
ruta_corta(Rutas,MejorRuta,ListaTramos,TiempoMínimo):-
    Rutas = [_,_|_],
    maplist(tiempo_ruta,Rutas,TiempoAristas,TiempoTotal),
    min_list(TiempoTotal,TiempoMínimo),
    nth0(N,TiempoTotal,TiempoMínimo),
    nth0(N,Rutas,MejorRuta),
    nth0(N,TiempoAristas,ListaTramos).

% calcula_mejor_ruta/4
% calcula_mejor_ruta(+EstaciónOrigen,+EstaciónDestino,-MejorRuta,-Tiempo)
% MejorRuta es una lista de estaciones que se deben seguir para llegar de EstaciónOrigen a
% EstaciónDestino. Tiempo es un número entero que representa el tiempo total de viaje.
calcula_mejor_ruta(EstaciónOrigen, EstaciónDestino, MejorRuta, Tiempo) :-
  %todas_rutas(EstaciónOrigen, EstaciónDestino, Rutas),
  máximo_número_de_rutas(RutasAGenerar),
  findnsols(RutasAGenerar,R,ruta(EstaciónOrigen,EstaciónDestino,R),Rutas),
  ruta_corta(Rutas,MejorRuta,_,Tiempo),!.

% mejor_ruta/3
% mejor_ruta(+EstaciónOrigen,+EstaciónDestino,-MejorRuta, -Tiempo)
% MejorRuta es una lista de estaciones que se deben seguir para llegar de EstaciónOrigen a
% EstaciónDestino. Tiempo es un número entero que representa el tiempo total de viaje.
mejor_ruta(EstaciónOrigen, EstaciónDestino, MejorRuta, Tiempo) :-
    calcula_mejor_ruta(EstaciónOrigen, EstaciónDestino, MejorRuta, Tiempo),
    format('Tiempo = '),
    imprimir_tiempo(Tiempo).

% imprimir_tiempo/1
% imprimir_tiempo(+Tiempo)
% Imprime el tiempo de viaje en formato de horas y minutos.
imprimir_tiempo(Tiempo):-
    Tiempo #> 60,
    Horas #= Tiempo // 60,
    Minutos #> 0,
    Minutos #= Tiempo mod 60,
    format('~w horas y ~w minutos~n', [Horas,Minutos]).
imprimir_tiempo(Tiempo):-
    Tiempo #< 60,
    format('~w minutos~n', [Tiempo]).
imprimir_tiempo(Tiempo):-
    Tiempo #> 0,
    Tiempo mod 60 #= 0,
    Horas #= Tiempo // 60,
    format('~w horas ~n', [Horas]).

% viene_antes/3
% viene_antes(+Estación1, +Estación2, +Línea)
% Verifica si Estación1 viene antes que Estación2 en la secuencia de estaciones de una línea.
viene_antes(Estación1, Estación2, Línea) :-
    sigue(Estación1, Estación2, Línea).
viene_antes(Estación1, Estación2, Línea) :-
    sigue(Estación1, Intermedia, Línea),
    viene_antes(Intermedia, Estación2, Línea).

% dirección/3
% dirección(+Estación1, +Estación2, -Dirección)
% Dirección es la dirección en la que se debe viajar para ir de Estación1 a Estación2.
dirección(Estación1, Estación2, Dirección) :-
    trayecto(Línea, Primera, Última),
    ( viene_antes(Estación1, Estación2, Línea) ->
        Dirección = Última
    ; viene_antes(Estación2, Estación1, Línea) ->
        Dirección = Primera
    ).

% Example usage:
% filter_stations_and_transfers([politecnico, instituto_del_petrolio, ...], [20-no,10-no,...], FilteredStations, FilteredTransfers).
% instrucciones/2
% instrucciones(+EstaciónOrigen,+EstaciónDestino)
% Imprime las instrucciones para llegar de EstaciónOrigen a EstaciónDestino en
% lenguaje natural.
instrucciones(EstaciónOrigen, EstaciónDestino):-
    calcula_mejor_ruta(EstaciónOrigen, EstaciónDestino, MejorRuta, _),
    tiempo_ruta(MejorRuta, TiempoAristas, _),

    MejorRuta = [EstaciónOrigen,SegundaEstación|RestoEstaciones],

    convertir_átomo(EstaciónOrigen, EstaciónOrígenTexto),
    format('Comenzar en estación ~w,~n',[EstaciónOrígenTexto]),

    (sigue(EstaciónOrigen, SegundaEstación, Línea); sigue(SegundaEstación, EstaciónOrigen, Línea)),
    color(Línea, ColorLínea),
    dirección(EstaciónOrigen,SegundaEstación, Dirección),

    convertir_átomo(Dirección, DirecciónTexto),
    convertir_átomo(ColorLínea, ColorTexto),
    texto_número_línea(Línea, LíneaTexto),

    format('Tomar la línea ~w(~w), con dirección hacia ~w~n', [ColorTexto, LíneaTexto, DirecciónTexto]),

    instrucciones_aux([SegundaEstación|RestoEstaciones], TiempoAristas),

    % extraemos la última estación de la ruta
    append(_,[ÚltimaEstación],MejorRuta),
    convertir_átomo(ÚltimaEstación, ÚltimaEstaciónTexto),
    format('hasta estación ~w.~n~n¡Fin del viaje!~n', [ÚltimaEstaciónTexto]),!.

% instrucciones_aux/2
% instrucciones_aux(+ListaEstaciones, +ListaTiemposTransbordo)
% Auxiliar recursivo para imprimir las instrucciones paso a paso.
instrucciones_aux([], []).
instrucciones_aux([_|RestoEstaciones],[_-no|RestoTiempos]):-
    instrucciones_aux(RestoEstaciones, RestoTiempos).
instrucciones_aux([EstaciónActual, PróximaEstación | RestoEstaciones], [_-Transbordo | RestoTiempos]) :-
    Transbordo \== no,
    convertir_átomo(EstaciónActual, EstaciónActualTexto),

    format('hasta estación ~w.~n~n', [EstaciónActualTexto]),

    (sigue(EstaciónActual, PróximaEstación, Línea); sigue(PróximaEstación, EstaciónActual, Línea)),
    color(Línea, ColorLínea),
    dirección(EstaciónActual, PróximaEstación, Dirección),

    convertir_átomo(Dirección, DirecciónTexto),
    convertir_átomo(ColorLínea, ColorTexto),
    texto_número_línea(Línea, LíneaTexto),

    format('Transbordar a la línea ~w(~w), con dirección hacia ~w~n', [ColorTexto, LíneaTexto, DirecciónTexto]),

    instrucciones_aux([PróximaEstación | RestoEstaciones], RestoTiempos).

% reporte_tiempo/2
% reporte_tiempo(+EstaciónOrigen,+EstaciónDestino)
% Imprime las instrucciones para llegar de EstaciónOrigen a EstaciónDestino en
% un formato con indicación por cada estación incluyendo transbordos y tiempo total de viaje.
reporte_tiempo(EstaciónOrigen, EstaciónDestino) :-
    calcula_mejor_ruta(EstaciónOrigen, EstaciónDestino, MejorRuta, Tiempo),

    format('~nTiempo total de viaje: ~w minutos = ',[Tiempo]),
    imprimir_tiempo(Tiempo),

    valor_parámetro(tiempo_inicial,TiempoInicial),
    MejorRuta = [InicioN|_],
    convertir_átomo(InicioN,Inicio),
    format('Inicio: ~w, ~w minutos~n',[Inicio,TiempoInicial]),

    tiempo_ruta(MejorRuta,TiempoTramos,_),
    once(imprimir_tramos(1,MejorRuta,TiempoTramos)),

    valor_parámetro(tiempo_final,TiempoFinal),
    append(_,[FinN],MejorRuta),
    convertir_átomo(FinN,Fin),
    format('Fin: ~w, ~w minutos~n~n',[Fin,TiempoFinal]).

% imprimir_tramos/3
% imprimir_tramos(+Posición,+Ruta,+Tramos)
% Imprime los tramos de una ruta en lenguaje natural.
imprimir_tramos(_,[_],_).
imprimir_tramos(Posición,[E1,E2|Ruta],[Tiempo-Transborde|Tramos]) :-
    Ruta \== [],
    (sigue(E2,E1,L1) ; sigue(E1,E2,L1)),
    (sigue(E2,_,L2) ; sigue(_,E2,L2)),
    L1 \== L2, % hay posibilidad de transborde

    convertir_átomo(E1,E1N), convertir_átomo(E2,E2N),

    ((Transborde == no,
    format('~w) ~w a ~w, ~w minutos, sin transborde~n',[Posición,E1N,E2N,Tiempo]));

    (Transborde \== no, convertir_átomo(Transborde,TransbordeN),
    format('~w) ~w a ~w, ~w minutos, transborde a ~w~n',[Posición,E1N,E2N,Tiempo,TransbordeN]))),

    PosiciónSiguiente #= Posición + 1,
    once(imprimir_tramos(PosiciónSiguiente,[E2|Ruta],Tramos)).
imprimir_tramos(Posición,[E1,E2|Ruta],[Tiempo-_|Tramos]) :-
    convertir_átomo(E1,E1N), convertir_átomo(E2,E2N),
    format('~w) ~w a ~w, ~w minutos~n',[Posición,E1N,E2N,Tiempo]),
    PosiciónSiguiente #= Posición + 1,
    once(imprimir_tramos(PosiciónSiguiente,[E2|Ruta],Tramos)).

% convertir_átomo/2
% convertir_átomo(+Átomo,-Texto)
% Convierte un átomo a un texto en lenguaje natural.
convertir_átomo(Átomo,Texto) :-
    atom_chars(Átomo,ListaÁtomo),
    once(agregar_espacios(ListaÁtomo,ÁtomoConEspacio)),
    agregar_mayúsculas(ÁtomoConEspacio,ÁtomoConMayúsculas),
    atom_chars(Texto,ÁtomoConMayúsculas).

% agregar_espacios/2
% agregar_espacios(+Átomo,-ÁtomoConEspacio)
% Agrega espacios a un átomo.
agregar_espacios(Átomo,Átomo) :-
    \+ member('_',Átomo).
agregar_espacios(Átomo,ÁtomoConEspacio) :-
    member('_',Átomo),
    nth0(N,Átomo,'_',Resto),
    nth0(N,NuevoÁtomo,' ',Resto),
    agregar_espacios(NuevoÁtomo,ÁtomoConEspacio).

% agregar_mayúsculas/2
% agregar_mayúsculas(+Átomo,-ÁtomoConMayúsculas)
% ÁtomoConMayúsculas es un Átomo dónde la primera letra de cada palabra está en mayúsculas.
agregar_mayúsculas(Átomo,ÁtomoConMayúsculas) :-
    \+ member(' ',Átomo),
    Átomo = [A|Resto],
    Letras = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
    member(A,Letras),
    upcase_atom(A,Mayúscula),
    ÁtomoConMayúsculas = [Mayúscula|Resto].
agregar_mayúsculas(Átomo,ÁtomoConMayúsculas) :-
    member(' ',Átomo),
    append(Inicio,[' '|Resto],Átomo),
    agregar_mayúsculas(Inicio,InicioN),
    agregar_mayúsculas(Resto,RestoN),
    append(InicioN,[' '|RestoN],ÁtomoConMayúsculas).
agregar_mayúsculas(Átomo,Átomo) :-
    Átomo = [A|_],
    Letras = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
    \+ member(A,Letras).

