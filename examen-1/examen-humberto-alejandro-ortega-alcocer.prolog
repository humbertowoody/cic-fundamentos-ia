% =============================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495.
%
% Examen - Metro de la Ciudad de México.
% =============================================================================

% =============================================================================
% Carga de la base de conocimiento con el grafo del metro
% =============================================================================
consult('metro-CDMX.prolog').

% =============================================================================
% Configuración de parámetros
% =============================================================================
valor_parámetro(tiempo_inicial, 8). % Valor inicial: 8
valor_parámetro(tiempo_tramo, 5). % Valor inicial: 5
valor_parámetro(tiempo_transbordo, 10). % Valor inicial: 10
valor_parámetro(tiempo_final, 7). % Valor inicial: 7

% =============================================================================
% Predicados Auxiliares
% =============================================================================


% =============================================================================
% Predicados Principales
% =============================================================================
test():-
  print('Prueba de la función "tiempo_total"'), nl.


