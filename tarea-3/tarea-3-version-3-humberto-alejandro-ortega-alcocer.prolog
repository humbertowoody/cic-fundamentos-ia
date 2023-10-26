% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Tarea 3 - Ejercicios de razonamiento lógico en Prolog.
%           Versión 3: 4 casas, 4 atributos y 9 pistas.
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

% entre2/3
% entre2(+C1, +C2, +Lista)
% Lista es una lista que contiene a C1, dos casas intermedias y C2, o en orden inverso.
entre2(C1, C2, Lista):- append(_, [C1, _, _, C2 | _], Lista).
entre2(C1, C2, Lista):- append(_, [C2, _, _, C1 | _], Lista).

% junto/3
% junto(+C1, +C2, +Lista)
% Lista es una lista que contiene a C1 y luego C2 o viceversa.
junto(C1, C2, Lista):- append(_, [C1, C2 | _], Lista).
junto(C1, C2, Lista):- append(_, [C2, C1 | _], Lista).

% posterior_a/3
% posterior_a(+C1, +C2, +Lista)
% C1 es posterior a C2 en la lista Lista.
posterior_a(C1, C2, Lista):- append(_, [C1 | R], Lista), member(C2, R).

% único/2
% único(+Pos, +Vecindario)
% Asegura que los valores del atributo en la posición Pos sean únicos en el Vecindario.
único(Pos, Vecindario) :-
    findall(Valor, (member(Casa, Vecindario), arg(Pos, Casa, Valor), nonvar(Valor)), Valores), % Encontramos todos los valores del atributo en la posición Pos.
    sort(Valores, ValoresÚnicos),                                                              % Quitamos los valores repetidos.
    length(Valores, LenValores),                                                               % Obtenemos la longitud de la lista de valores.
    length(ValoresÚnicos, LenValoresÚnicos),                                                   % Obtenemos la longitud de la lista de valores únicos.
    LenValores =:= LenValoresÚnicos.                                                           % Si son iguales, entonces todos los valores son únicos.

% visualiza_vecindario/1
% visualiza_vecindario(-V)
% V es una lista de cuatro casas que cumplen las siguientes condiciones:
visualiza_vecindario(V):-
  V = [_,_,_,_],                                                % El vecindario tiene cuatro casas.
  entre2(casa(_, _, boliche, _), casa(_, _, natación, _), V),   % 1 Hay dos casas entre la del bolichista y la del nadador...
  entre(casa(irlandés, _, _, _), casa(_, _, volleyball, _), V), % 2 Hay una casa entre la del irlandés y la del que juega voleyball...
  V = [_, casa(_, negro, _, _), _, _],                          % 3 La segunda casa es negra...
  entre(casa(_, _, _, caballos), casa(_, rojo, _, _), V),       % 4 Hay una casa entre la del dueño de caballos y la casa roja...
  junto(casa(escocés, _, _, _), casa(_, _, _, tortugas), V),    % 5 Un escocés vive junto al dueño de tortugas...
  entre2(casa(_, _, _, caballos), casa(_, _, _, mariposas), V), % 6 Hay dos casas entre la del dueño de caballos y la casa del dueño de mariposas...
  posterior_a(casa(_, _, tennis, _), casa(_, _, boliche, _), V),% 7 El bolichista vive en algún lugar posterior a la casa del tenista...
  entre(casa(_, _, volleyball, _), casa(_, blanco, _, _), V),   % 8 Hay una casa entre la del que juega voleyball y la casa blanca...
  V = [casa(ruso, _, _, _) | _ ],                               % 9 Un ruso vive en la primera casa...
  % La siguiente invocación sólamente sirve para asegurarnos de que sólo existe uno de los atributos en la lista final.
  % Usé un forall/2 porque es más fácil que llamar a único/2 para cada atributo.
  forall(between(1, 4, Pos), único(Pos, V)).

