
:-debug.

% tiempo en segundos, posteriromente hay q cambiarlo por algo que sea como una variable, 
% usando modificacion de la bd
time(3).
% igual que el anterior
table_dim(5, 8).

% corral(X, Y).
% dirty(X, Y).
% nino(X, Y, id).
% robot(X, Y, id, carga).

% verifica si esta en la tabla las coordenadas X, Y
in_table(X, Y):- table_dim(N, M), X<N, Y<M, 0=<X, 0=<Y.
% actualiza la base de datos de prolog, el termino T con los parametros O, lo elimima, y 
% y agrega el termino T con los parametros N.
act(T, O, N):- C=..[T|O], C, retract(C), C1=..[T|N], assertz(C1).

