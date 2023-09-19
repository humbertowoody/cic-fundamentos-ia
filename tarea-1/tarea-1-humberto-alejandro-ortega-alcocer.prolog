%========================================================================================
% Humberto Alejandro Ortega Alcocer, ESCOM, 2016630495
% 
% Tarea 1 - Arbol Genealógico.
%           Construir un programa Prolog para representar las relaciones de parentesco en 
%           la familia Simpson. Programar las reglas para las opciones de: medios hermanos,
%           tíos, primos, cuñados y ancestros en general.
%
% La imagen de referencia del árbol genealógico de la familia Simpson utilizado:
% https://simpsons.fandom.com/wiki/Simpson_family?file=Simpsons_possible_family_tree.jpg
%
% Algunas propuestas de ejecución:
%
%  - padre_de(X,Y).            ~ Todas las relaciones de padres.
%  - madre_de(X,Y).            ~ Todas las relaciones de madres.
%  - abuelo_de(X,Y).           ~ Todas las relaciones de abuelos.
%  - abuela_de(X,Y).           ~ Todas las relaciones de abuelas.
%  - hermano_de(X,Y).          ~ Todas las relaciones de hermanos.
%  - hermana_de(X,Y).          ~ Todas las relaciones de hermanas.
%  - medio_hermano_de(X,Y).    ~ Todas las relaciones de medios hermanos.
%  - media_hermana_de(X,Y).    ~ Todas las relaciones de medias hermanas.
%  - tío_de(X,Y).              ~ Todas las relaciones de tío.
%  - tía_de(X,Y).              ~ Todas las relaciones de tía.
%  - primo_de(X,Y).            ~ Todas las relaciones de primos.
%  - prima_de(X,Y).            ~ Todas las relaciones de primas.
%  - cuñado_de(X,Y).           ~ Todas las relaciones de cuñados.
%  - cuñada_de(X,Y).           ~ Todas las relaciones de cuñadas
%  - ancestro(bart, Ancestro). ~ Todos los ancestros de Bart.
% 
% Fundamentos de Inteligencia Artificial, CIC, IPN, 2023.
%========================================================================================


%========================================================================================
% Base de Conocimientos.
%
% Definición de género.
%========================================================================================

hombre(virgil).
hombre(abraham_i).
hombre(hugo).
hombre(old_tut).
hombre(orville).
hombre(chet).
hombre(cyrus).
hombre(tyrone).
hombre(abraham_ii).
hombre(clancy).
hombre(herb).
hombre(homer).
hombre(bart).

mujer(mabel).
mujer(eliza).
mujer(hortense).
mujer(mona).
mujer(jackie).
mujer(abbey).
mujer(marge).
mujer(patty).
mujer(selma).
mujer(lisa).
mujer(maggie).
mujer(ling).

%========================================================================================
% Definición de relación genealógica fundamental.
%========================================================================================

desciende(virgil, eliza).
desciende(mabel, eliza).
desciende(virgil, abraham_i).
desciende(mabel, abraham_i).

desciende(abraham_i, hugo).
desciende(abraham_i, old_tut).

desciende(old_tut, orville).

desciende(orville, chet).
desciende(orville, hortense).
desciende(orville, cyrus).
desciende(orville, tyrone).
desciende(orville, abraham_ii).

desciende(abraham_ii, herb).
desciende(abraham_ii, homer).
desciende(mona, homer).
desciende(abraham_ii, abbey).

desciende(clancy, marge).
desciende(jackie, marge).
desciende(clancy, patty).
desciende(jackie, patty).
desciende(clancy, selma).
desciende(jackie, selma).

desciende(homer, bart).
desciende(marge, bart).
desciende(homer, lisa).
desciende(marge, lisa).
desciende(homer, maggie).
desciende(marge, maggie).

desciende(selma, ling).

%========================================================================================
% Definición de predicados lógicos para análisis genealógico (functores).
%========================================================================================

%========================================================================================
% padre_de/2
% padre_de(+Personaje, -Padre)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todos los padres.
% 
% Definición de relación genealógica: el padre es un progenitor de sexo masculino.
%========================================================================================
padre_de(X, Padre):-
    desciende(Padre, X),
    hombre(Padre).

%========================================================================================
% madre_de/2
% madre_de(+Personaje, -Madre)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todas las madres.
% 
% Definición de relación genealógica: la madre es una progenitora de sexo femenino.
%========================================================================================
madre_de(X, Madre):-
    desciende(Madre, X),
    mujer(Madre).

%========================================================================================
% abuelo_de/2
% abuelo_de(+Personaje, -Abuelo)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todos los abuelos.
% 
% Definición de relación genealógica: el abuelo es el progenitor masculino de alguno 
%                                     de los progenitores directos.
%========================================================================================
abuelo_de(X, Abuelo):-
    desciende(Ancestro, X),
    padre_de(Ancestro, Abuelo).


%========================================================================================
% abuela_de/2
% abuela_de(+Personaje, -Abuela)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todas las abuelas.
% 
% Definición de relación genealógica: la abuela es la progenitora femenina de alguno 
%                                     de los progenitores directos.
%========================================================================================
abuela_de(X, Abuela):-
    desciende(Ancestro, X),
    madre_de(Ancestro, Abuela).

%========================================================================================
% hermano_de/2
% hermano_de(+Personaje, -Hermano)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todos los hermanos hombres.
% 
% Definición de relación genealógica: el hermano es el individuo que comparte un progenitor 
%                                     directo en común.
%========================================================================================
hermano_de(X, Hermano):-
    desciende(A,X),
    desciende(A,Hermano),
    Hermano \== X,
    hombre(Hermano).

%========================================================================================
% hermana_de/2
% hermana_de(+Personaje, -Hermana)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todas las hermanas mujeres.
% 
% Definición de relación genealógica: la hermana es el individuo que comparte un progenitor 
%                                     directo en común.
%========================================================================================
hermana_de(X, Hermana):-
    desciende(A, X),
    desciende(A, Hermana),
    Hermana \== X,
    mujer(Hermana).


%========================================================================================
% medio_hermano_de/2
% medio_hermano_de(+Personaje, -MedioHermano)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todos los medios hermanos hombres.
% 
% Definición de relación genealógica: el medio hermano es el individuo que comparte un progenitor 
%                                     directo en común más no el segundo.
%========================================================================================
medio_hermano_de(X, MedioHermano):-
    desciende(A,X),
    desciende(A,MedioHermano),
    desciende(B,X),
    \+desciende(B,MedioHermano),
    MedioHermano \== X,
    hombre(MedioHermano).

%========================================================================================
% media_hermana_de/2
% media_hermana_de(+Personaje, -MediaHermana)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todas las medias hermanas mujeres.
% 
% Definición de relación genealógica: la media hermana es el individuo que comparte un progenitor 
%                                     directo en común más no el segundo.
%========================================================================================
media_hermana_de(X, MediaHermana):-
    desciende(A,X),
    desciende(A,MediaHermana),
    desciende(B,X),
    \+desciende(B,MediaHermana),
    MediaHermana \== X,
    mujer(MediaHermana).

%========================================================================================
% tío_de/2
% tío_de(+Personaje, -Tío)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todos los tíos hombres.
% 
% Definición de relación genealógica: el tío es el hermano de algún progenitor.
%========================================================================================
tío_de(X, Tío):-
    desciende(A,X),
    hermano_de(A,Tío).

%========================================================================================
% tía_de/2
% tía_de(+Personaje, -Tía)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todas las tías mujeres.
% 
% Definición de relación genealógica: la tía es la hermana de algún progenitor.
%========================================================================================
tía_de(X, Tía):-
    desciende(A,X),
    hermana_de(A,Tía).

%========================================================================================
% primo_de/2
% primo_de(+Personaje, -Primo)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todos los primos hombres.
% 
% Definición de relación genealógica: el primo es el hijo de algún tío o tía.
%========================================================================================
primo_de(X, Primo):-
    ((tía_de(X, Tía),
    madre_de(Primo, Tía));
    (tío_de(X, Tío),
    padre_de(Primo, Tío))),
    hombre(Primo).

%========================================================================================
% prima_de/2
% prima_de(+Personaje, -Prima)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todas las primas mujeres.
% 
% Definición de relación genealógica: la prima es la hija de algún tío o tía.
%========================================================================================
prima_de(X, Prima):-
    ((tía_de(X, Tía),
    madre_de(Prima, Tía));
    (tío_de(X, Tío),
    padre_de(Prima, Tío))),
    mujer(Prima).

%========================================================================================
% cuñado_de/2
% cuñado_de(+Personaje, -Cuñado)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todos los cuñados hombres.
% 
% Definición de relación genealógica: el cuñado es la pareja hombre de algún hermano, o bien,
%                                     los hermanos de la pareja del personaje.
%========================================================================================
cuñado_de(X, Cuñado):-
   (hermana_de(X, Hermana),
    madre_de(Hermana, Sobrino),
    padre_de(Sobrino, Cuñado)) ;
   (desciende(X, Hijo),
    desciende(Y, Hijo),
    X \== Y,
    hermano_de(Y, Cuñado)).
    
%========================================================================================
% cuñada_de/2
% cuñada_de(+Personaje, -Cuñada)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todas las cuñadas mujeres.
% 
% Definición de relación genealógica: la cuñada es la pareja mujer de algún hermano, o bien,
%                                     las hermanas de la pareja del personaje.
%========================================================================================
cuñada_de(X, Cuñada):-
   (hermano_de(X, Hermano),
    padre_de(Hermano, Sobrino),
    madre_de(Sobrino, Cuñada)) ;
   (desciende(X, Hijo),
    desciende(Y, Hijo),
    X \== Y,
    hermana_de(Y, Cuñada)).

%========================================================================================
% ancestro/2
% ancestro(+Personaje, -Ancestro)
%
% Recibe como parámetro el nombre de algún personaje conocido y devuelve el nombre 
% de todos los ancestros.
% 
% Definición de relación genealógica: el ancestro es un individuo que se encuentra 
%                                     relacionado en el árbol genealógico por medio de 
%                                     algún progenitor con el personaje.
%========================================================================================
ancestro(X,Ancestro):- desciende(Ancestro, X).
ancestro(X,Ancestro):- desciende(Z, X),
                       ancestro(Z, Ancestro).

% Fin del archivo.
