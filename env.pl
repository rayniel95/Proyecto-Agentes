:- use_module(utils).
:-debug.
% tiempo en segundos, posteriromente hay q cambiarlo por algo que sea como una variable, 
% usando modificacion de la bd
time(3).
% igual que el anterior
table_dim(5, 8).

% p(x, y). define un punto en el tablero
% dado un punto inicial P y las dimensiones de un tablero, devuelve todos aquellos puntos 
% hacia donde es valido moverse, abajo, izquierda, derecha, arriba
valid(point(Xp, Yp), table_dim(N, M), point(Xr, Yr)):- (Xr is Xp+1, Xr<N, Yr is Yp); 
    (Xr is Xp, Yr is Yp+1, Yr<M); (Xr is Xp, Yr is Yp-1, Yr>=0); (Xr is Xp-1, Xr>=0, Yr is Yp).

haz_corral(P0, table_dim(N, M), 0, []).
haz_corral(point(X0, Y0), table_dim(N, M), 1, [point(X0, Y0)]):- X0 is random(N), 
    Y0 is random(M), !.

haz_corral(P0, table_dim(N, M), K, T):- K1 is K-1, haz_corral(P, table_dim(N, M), K1, L1),
    valid(P, table_dim(N, M), P1), P0=P1, not(member(P0, L1)), append(L1, [P0], T1), T=T1.


generate(T, point(Xp, Yp)):- table_dim(N,M), Xp is random(N), Yp is random(M).


obstacles(L, table_dim(N,M), 0, []).
obstacles(L, table_dim(N, M), 1, [P]):- generate(table_dim(N,M), P), not(member(P, L)), !.

obstacles(L, table_dim(N, M), K, [P|R]):- K1 is K-1, obstacles(L, table_dim(N,M), K1, R),
    generate(table_dim(N, M), P), not(member(P, L)), not(member(P, R)).

