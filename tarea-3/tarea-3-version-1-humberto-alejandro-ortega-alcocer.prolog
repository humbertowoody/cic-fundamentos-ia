% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Tarea 3 - Ejercicios de razonamiento lógico en Prolog.
%           Versión 1: 3 casas, 2 atributos y 3 pistas.
%
% Propuesta de Ejecución:
%   1. Ejecutar el predicado visualiza_vecindario/1.
%   2. El programa ofrecerá las soluciones posibles.
%
% Fundamentos de Inteligencia Artificial, CIC, IPN, 2023.
% =============================================================================

% =============================================================================
% Modelado de una casa para este problema:
% casa(nacionalidad,color)
% =============================================================================


% =============================================================================
% Predicados lógicos
% =============================================================================

% junto/3
% junto(+C1, +C2, -Lista)
% Lista es una lista que contiene a C1 y luego C2 o viceversa.
junto(C1, C2, Lista):- append(_, [C1, C2 | _], Lista).
junto(C1, C2, Lista):- append(_, [C2, C1 | _], Lista).

% visualiza_vecindario/1
% visualiza_vecindario(-V)
% V es una lista de tres casas que cumplen las siguientes condiciones:
visualiza_vecindario(V):-
  V = [_,_,_],                             % El vecindario tiene tres casas.
  junto(casa(español,_), casa(_,rojo), V), % 1 El español vive junto a la casa roja.
  member(casa(noruego,azul), V),           % 2 El noruego vive en la casa azul.
  V = [_, casa(italiano,_), _].            % 3 El italiano vive en la segunda casa.

