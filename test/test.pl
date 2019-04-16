:-debug.

table_dim(4, 5).

% append([],X,[X]).
% ppend([X|Y],Z,[X|W]):- append(Y,Z,W). 

valid(point(Xp, Yp), table_dim(N, M), point(Xr, Yr)):- (Xr is Xp+1, Xr<N, Yr is Yp); 
    (Xr is Xp, Yr is Yp+1, Yr<M); (Xr is Xp, Yr is Yp-1, Yr>=0); (Xr is Xp-1, Xr>=0, Yr is Yp).

haz_corral(P0, table_dim(N, M), 0, []).
haz_corral(point(X0, Y0), table_dim(N, M), 1, [point(X0, Y0)]):- X0 is random(N), 
    Y0 is random(M), !.

haz_corral(P0, table_dim(N, M), K, T):- K1 is K-1, haz_corral(P, table_dim(N, M), K1, L1),
    valid(P, table_dim(N, M), P1), P0=P1, not(member(P0, L1)), append(L1, [P0], T1), T=T1.