% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Tarea 3 - Ejercicios de razonamiento lógico en Prolog.
%           Versión 2: 3 casas, 4 atributos y 6 pistas.
%
% Propuesta de Ejecución:
%   1. Ejecutar el predicado visualiza_vecindario/1.
%   2. El programa ofrecerá las soluciones posibles.
%
% Fundamentos de Inteligencia Artificial, CIC, IPN, 2023.
% =============================================================================

% =============================================================================
% Modelado de una casa para este problema:
% casa(nacionalidad,color,deporte,mascota)
% =============================================================================


% =============================================================================
% Predicados lógicos
% =============================================================================

% entre/3
% entre(+C1, +C2, +Lista)
% Lista es una lista que contiene a C1, una casa intermedia y C2, o en orden inverso.
entre(C1, C2, Lista):- append(_, [C1, _, C2 | _], Lista).
entre(C1, C2, Lista):- append(_, [C2, _, C1 | _], Lista).

% junto/3
% junto(+C1, +C2, -Lista)
% Lista es una lista que contiene a C1 y luego C2 o viceversa.
junto(C1, C2, Lista):- append(_, [C1, C2 | _], Lista).
junto(C1, C2, Lista):- append(_, [C2, C1 | _], Lista).

% visualiza_vecindario/1
% visualiza_vecindario(-V)
% V es una lista de tres casas que cumplen las siguientes condiciones:
visualiza_vecindario(V):-
  V = [_,_,_],                                                                   % El vecindario tiene tres casas.
  (V = [casa(brasileño, _, _, _), _, _] ; V = [_, _, casa(brasileño, _, _, _)]), % 1 El brasileño NO vive en la segunda casa...
  member(casa(_, _, baloncesto, perros), V),                                     % 2 El dueño de perros juega baloncesto...
  entre(casa(_, _, fútbol, _), casa(_, rojo, _, _), V),                          % 3 Hay una casa intermedia entre la del que juega fútbol y la casa roja...
  junto(casa(_, _, _, peces), casa(_, _, _, gatos), V),                          % 4 El dueño de peces vive junto al dueño de gatos...
  junto(casa(_, _, _, perros), casa(_, verde, _, _), V),                         % 5 El dueño de perros vive junto a la casa verde...
  V = [_, _, casa(alemán, _, _, _)].                                             % 6 Un alemán vive en la tercera casa...

