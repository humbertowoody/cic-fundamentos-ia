% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Tarea 4 - Misioneros y Caníbales.
%           Utilizando el algoritmo de búsqueda en anchura, profundidad y
%           profundidad iterativa, resuelve el problema de los misioneros y
%           caníbales.
%
% Representación de los estados:
%  - [MO,CO,MD,CD,L]
%    ~ MO: Número de misioneros en la orilla izquierda.
%    ~ CO: Número de caníbales en la orilla izquierda.
%    ~ MD: Número de misioneros en la orilla derecha.
%    ~ CD: Número de caníbales en la orilla derecha.
%    ~ L:  Indica la posición de la barca. 'o' para orilla izquierda y 'd' para
%          orilla derecha.
%
% Propuesta de Ejecución:
%   - Para la búsqueda en anchura:
%     1. Ejecutar el predicado busca_BFS/3.
%     2. El programa ofrecerá las soluciones posibles.
%     3. Seleccionar la solución deseada.
%     4. Usar el predicado despliega/1 para mostrar la solución.
%     Ejemplo:
%       ?- busca_BFS([3,3,0,0,o],[0,0,3,3,d],Plan), despliega(Plan).
%   - Para la búsqueda en profundidad:
%     1. Ejecutar el predicado busca_DFS/3.
%     2. El programa ofrecerá las soluciones posibles.
%     3. Seleccionar la solución deseada.
%     4. Usar el predicado despliega/1 para mostrar la solución.
%     Ejemplo:
%       ?- busca_DFS([3,3,0,0,o],[0,0,3,3,d],Plan), despliega(Plan).
%   - Para la búsqueda en profundidad iterativa:
%     1. Ejecutar el predicado busca_IDS/3.
%     2. El programa ofrecerá las soluciones posibles.
%     3. Seleccionar la solución deseada.
%     4. Usar el predicado despliega/1 para mostrar la solución.
%     Ejemplo:
%       ?- busca_IDS([3,3,0,0,o],[0,0,3,3,d],Plan), despliega(Plan).
%
% Fundamentos de Inteligencia Artificial, CIC, IPN, 2023.
% =============================================================================

% edo_válido/4
% edo_válido(+estado)
% Verdadero si el estado proporcionado es válido. Es decir, si cumple con las
% restricciones del problema de los misioneros y caníbales.
edo_válido([Mis1,Can1,Mis2,Can2]) :-
    Mis1 >= 0, Can1 >= 0,
    Mis2 >= 0, Can2 >= 0,
    (Mis1 >= Can1 ; Mis1 is 0),
    (Mis2 >= Can2 ; Mis2 is 0).


% movimiento/2
% movimiento(+estado,-estado)
% Verdadero si existe un estado inicial tal que al aplicar un movimiento se pueda
% llegar a un estado destino que sea válido.
movimiento([MO,CO,MD,CD,L1],[MO2,CO2,MD2,CD2,L2]) :-
    % Un misionero.
    ( (MO2 is MO - 1, CO2 is CO, MD2 is MD + 1, CD2 is CD, L1 = 'o', L2 = 'd');
      (MO2 is MO + 1, CO2 is CO, MD2 is MD - 1, CD2 is CD, L1 = 'd', L2 = 'o');
    % Dos misioneros.
      (MO2 is MO - 2, CO2 is CO, MD2 is MD + 2, CD2 is CD, L1 = 'o', L2 = 'd');
      (MO2 is MO + 2, CO2 is CO, MD2 is MD - 2, CD2 is CD, L1 = 'd', L2 = 'o');
    % Misionero y caníbal.
      (MO2 is MO - 1, CO2 is CO - 1, MD2 is MD + 1, CD2 is CD + 1, L1 = 'o', L2 = 'd');
      (MO2 is MO + 1, CO2 is CO + 1, MD2 is MD - 1, CD2 is CD - 1, L1 = 'd', L2 = 'o');
    % Un caníbal.
      (MO2 is MO, CO2 is CO - 1, MD2 is MD, CD2 is CD + 1, L1 = 'o', L2 = 'd');
      (MO2 is MO, CO2 is CO + 1, MD2 is MD, CD2 is CD - 1, L1 = 'd', L2 = 'o');
    % Dos caníbales.
      (MO2 is MO, CO2 is CO - 2, MD2 is MD, CD2 is CD + 2, L1 = 'o', L2 = 'd');
      (MO2 is MO, CO2 is CO + 2, MD2 is MD, CD2 is CD - 2, L1 = 'd', L2 = 'o') ),
    % Verificación de que el estado destino sea válido.
    edo_válido([MO2,CO2,MD2,CD2]).

% sucesores/2
% sucesores(+estado,-sucesores)
% Sucesores es una lista con los estados sucesores del estado proporcionado.
sucesores([Edo|Resto], Sucesores) :-
    findall([S,Edo|Resto], (movimiento(Edo,S), \+member(S,[Edo|Resto])),Sucesores).

% busca_BFS/3
% busca_BFS(+estado_inicial, +estado_meta, -plan)
% Verdadero si plan es una lista con los estados requeridos para llegar del estado
% inicial al estado meta. Se usa reverse para que el plan se muestre en el orden
% correcto.
busca_BFS(Ei,Em,Plan) :-
    bfs(Em,[[Ei]],Ruta),
    reverse(Ruta,Plan).

% bfs/3
% bfs(+estado_meta, +agenda, -ruta)
% Verdadero si ruta es una lista con los estados requeridos para llegar del estado
% inicial, con el cual inicia la agenda, al estado meta. Utilizando una agenda de
% tipo cola (FIFO). La búsqueda se realiza en anchura.
bfs(Meta,[[Meta|Trayecto]|_],[Meta|Trayecto]).
bfs(Meta,[Candidato|Frontera],Ruta) :-
    sucesores(Candidato,Suc),
    append(Frontera,Suc,NuevaAgenda),
    bfs(Meta,NuevaAgenda,Ruta).

% busca_DFS/3
% busca_DFS(+estado_inicial, +estado_meta, -plan)
% Verdadero si plan es una lista con los estados requeridos para llegar del estado
% inicial al estado meta. Se usa reverse para que el plan se muestre en el orden
% correcto.
busca_DFS(Ei,Em,Plan) :-
    dfs(Em,[[Ei]],Ruta),
    reverse(Ruta,Plan).

% dfs/3
% dfs(+estado_meta, +agenda, -ruta)
% Verdadero si ruta es una lista con los estados requeridos para llegar del estado
% inicial, con el cual inicia la agenda, al estado meta. Utilizando una agenda de
% tipo pila (LIFO). La búsqueda se realiza en profundidad.
dfs(Meta,[[Meta|Trayecto]|_],[Meta|Trayecto]).
dfs(Meta,[Candidato|Frontera],Ruta) :-
  sucesores(Candidato,Suc),
  append(Suc,Frontera,NuevaAgenda),
  dfs(Meta,NuevaAgenda,Ruta).

% edo_meta/1
% edo_meta(+estado)
% Verdadero si estado es el estado meta.
% NOTA: Este predicado es utilizado para almacenar el estado meta durante la
%       busqueda en profundidad iterativa (IDS).
:- dynamic(edo_meta/1).

% busca_IDS/3
% busca_IDS(+estado_inicial, +estado_meta, -plan)
% Verdadero si plan es una lista con los estados requeridos para llegar del estado
% inicial al estado meta. Se usa reverse para que el plan se muestre en el orden
% correcto. Se utiliza dfs/2 para realizar la busqueda en profundidad iterativa.
busca_IDS(Ei,Em,Plan) :-
    retractall(edo_meta(_)),
    assert(edo_meta(Em)),
    dfs([[Ei]],Ruta),
    reverse(Ruta,Plan).

% dfs/2
% dfs(+agenda, -ruta)
% Verdadero si ruta es una lista con los estados requeridos para llegar del estado
% inicial, con el cual inicia la agenda, al estado meta. Utilizando una agenda de
% tipo pila (LIFO). La búsqueda se realiza en profundidad (DFS), pero es iterativa
% debido a que se realiza de manera ordenada.
dfs([[Meta|Trayecto]|_],[Meta|Trayecto]) :- edo_meta(Meta).
dfs([Candidato|Frontera],Ruta) :-
    sucesores(Candidato,Suc),
    append(Suc,Frontera,NuevaAgenda),
    dfs(NuevaAgenda,Ruta).

% nombre_movimiento/3
% nombre_movimiento(+estado inicio,+estado destino,-movimiento)
% Movimiento es una cadena de texto que indica el tipo de movimiento que se realizó
% para llegar del estado inicio al estado destino.
nombre_movimiento([MO,CO,MD,CD,L1],[MO2,CO2,MD2,CD2,L2],Movimiento) :-
  % Un misionero
  ( ((MO2 is MO - 1, CO2 is CO, MD2 is MD + 1, CD2 is CD, L1 = 'o', L2 = 'd'),Movimiento = 'UN-MISIONERO');
    ((MO2 is MO + 1, CO2 is CO, MD2 is MD - 1, CD2 is CD, L1 = 'd', L2 = 'o'),Movimiento = 'UN-MISIONERO');
  % Dos misioneros
    ((MO2 is MO - 2, CO2 is CO, MD2 is MD + 2, CD2 is CD, L1 = 'o', L2 = 'd'),Movimiento = 'DOS-MISIONEROS');
    ((MO2 is MO + 2, CO2 is CO, MD2 is MD - 2, CD2 is CD, L1 = 'd', L2 = 'o'),Movimiento = 'DOS-MISIONEROS');
  % Misionero y caníbal
    ((MO2 is MO - 1, CO2 is CO - 1, MD2 is MD + 1, CD2 is CD + 1, L1 = 'o', L2 = 'd'),Movimiento = 'MISIONERO-Y-CANÍBAL');
    ((MO2 is MO + 1, CO2 is CO + 1, MD2 is MD - 1, CD2 is CD - 1, L1 = 'd', L2 = 'o'),Movimiento = 'MISIONERO-Y-CANÍBAL');
  % Un caníbal
    ((MO2 is MO, CO2 is CO - 1, MD2 is MD, CD2 is CD + 1, L1 = 'o', L2 = 'd'),Movimiento = 'UN-CANÍBAL');
    ((MO2 is MO, CO2 is CO + 1, MD2 is MD, CD2 is CD - 1, L1 = 'd', L2 = 'o'),Movimiento = 'UN-CANÍBAL');
  % Dos caníbales
    ((MO2 is MO, CO2 is CO - 2, MD2 is MD, CD2 is CD + 2, L1 = 'o', L2 = 'd'),Movimiento = 'DOS-CANÍBALES');
    ((MO2 is MO, CO2 is CO + 2, MD2 is MD, CD2 is CD - 2, L1 = 'd', L2 = 'o'),Movimiento = 'DOS-CANÍBALES')),
  edo_válido([MO2,CO2,MD2,CD2]).

% despliega_pasos/2
% despliega_pasos(+plan,+número de movimiento)
% Imprime el número de paso seguido del movimiento realizado para llegar al estado
% destino.
despliega_pasos([Origen,Destino],N) :-
  Destino = [MO,CO,MD,CD,L1],
  ((L1 = o, Barca_origen is 1, Barca_destino is 0) ; (L1 = d, Barca_origen is 0, Barca_destino is 1)),
  nombre_movimiento(Origen,Destino,Movimiento),
  format('(~w)~t aplicando ~w~t se llega a ((~w ~w ~w) (~w ~w ~w))\n', [N,Movimiento,MO,CO,Barca_origen,MD,CD,Barca_destino]),!.
despliega_pasos([Origen,Destino|Resto],N) :-
  Destino \= [],
  Resto \= [],
  Destino = [MO,CO,MD,CD,L1],
  ((L1 = o, Barca_origen is 1, Barca_destino is 0) ; (L1 = d, Barca_origen is 0, Barca_destino is 1)),
  nombre_movimiento(Origen,Destino,Movimiento),
  format('(~w)~t Aplicando ~w~t se llega a ((~w ~w ~w) (~w ~w ~w))\n', [N,Movimiento,MO,CO,Barca_origen,MD,CD,Barca_destino]),
  M is N + 1,
  despliega_pasos([Destino|Resto],M).

% despliega/1
% despliega(+plan)
% Impresión de la solución del problema de los misioneros y caníbales.
despliega(Plan) :-
  length(Plan, Tamaño),
  Num_Pasos is Tamaño - 1,
  Plan = [Inicio|_],
  Inicio = [MO,CO,MD,CD,L1],
  format('\nÉxito. Solución con ~w pasos:\n', [Num_Pasos]),
  ((L1 = o, Barca_origen is 1, Barca_destino is 0) ; (L1 = d, Barca_origen is 0, Barca_destino is 1)),
  format('Inicio en: (~w ~w ~w) (~w ~w ~w)\n', [MO,CO,Barca_origen,MD,CD,Barca_destino]),
  despliega_pasos(Plan,1).

