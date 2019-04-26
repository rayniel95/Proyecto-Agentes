bfs([], _, _).
bfs([(D,X,Y)|R], Cond, Distancia) :- Xr is X-1, Xb is X+1,
                    Yd is Y+1, Yi is Y-1,
                    filter([(X,Yi), (Xr,Y), (X,Yd), (Xb,Y),
                            (Xr,Yi), (Xr,Yd), (Xb,Yi), (Xb,Yd)],
                          Cond, L),
                    Di is D+1, convertir(L, Di, Cv, (X, Y), Distancia),
                    append(R, Cv, Z), bfs(Z, Cond, Distancia).