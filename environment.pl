% construye una lista de K corrales que forman una region conexa en el mapa, se selecciona un
% punto random
% K:int, obligatory
% L:list
build_jail(K, L):- build_jail_(_, K, L).

build_jail_(jail(_, _), 0, []).
build_jail_(jail(X0, Y0), 1, [jail(X0, Y0)]):- generate(X0, Y0), !.
build_jail_(jail(X0, Y0), K, [jail(X0, Y0)|R]):- K1 is K-1, build_jail_(jail(X, Y), K1, R), 
    valid_point(X, Y, X0, Y0), not(member(jail(X0, Y0), R)).

% triunfa si el termino T, siendo considerado como un punto en el tablero, no se encuentra 
% en la lista L
% L:list
% T:term
% point_not_in([], T).
% point_not_in([X|R], T):- arg(1, X, X0), arg(2, X, Y0), not((arg(1, T, X0), arg(2, T, Y0))),
%     point_not_in(R, T).

% el problema del falso fue arreglado con una lista que contiene los puntos que ya son
% validos, esto puede dar overflow en algun momwnto, para tableros muy grandes, pero en lo
% general funciona.
ran_uniq_disj_point(_, 0, [], _, _):- true, !.
ran_uniq_disj_point(L, 1, [P], T, A):- to_list_point(L, R), select_ran_point(R, P1),
    functor(P, T, A), P1=[X, Y], init_point(P, X, Y), !.
ran_uniq_disj_point(L, K, [P|R], T, A):- K1 is K-1, ran_uniq_disj_point(L, K1, R, T, A), 
    append(L, R, L1), to_list_point(L1, Lp), select_ran_point(Lp, P1), P1=[X, Y], 
    functor(P, T, A), init_point(P, X, Y).

% se encarga de hacer la suciedad acorde al documento, selecciona random una de las 8 
% casillas adyacentes, si es valida para ensuciar se ensucia, si no, no se ensucia y vuelve
% repetir el mismo procedimiento, este se realiza K veces, con K casillas distintas
% X:int, obligatory
% Y:int, obligatory
% K:int, obligatory
% L:list
make_dirt_(_, _, _, 0):- true, !.
make_dirt_(X, Y, L, K):- select_ran_item(L, Dir), select(Dir, R, L), !, 
    mov_gradient(X, Y, Xn, Yn, Dir), ((is_valid_for_dirt(Xn, Yn), dirt(Xn, Yn)); true), 
    K1 is K-1, make_dirt_(X, Y, R, K1).
% el que llama tiene que asegurarse de que el nino no se enctuentre en una casilla de corral
make_dirt(X, Y, K):- make_dirt_(X, Y, [arriba, abajo, izquierda, derecha, arriba_izquierda, 
    arriba_derecha, abajo_izquierda, abajo_derecha], K), !.

% dado un centro en el tablero X, Y cuenta la cantidad K de elementos, que tienen functor de
% Term, en las direcciones de la lista L.
% X:int, obligatory
% Y:int, obligatory
% F:term, obligatory
% K:int
count_elem_(_, _, [], _, 0).
count_elem_(X, Y, [Dir], Term, K):- K is 0, mov_gradient(X, Y, Xn, Yn, Dir), 
    not(exist_point(Term, Xn, Yn)), !.
count_elem_(X, Y, [Dir], Term, K):- K is 1, mov_gradient(X, Y, Xn, Yn, Dir), 
    exist_point(Term, Xn, Yn), !.
count_elem_(X, Y, [Dir|R], Term, K):- count_elem_(X, Y, R, Term, K1),
    mov_gradient(X, Y, Xn, Yn, Dir), ((exist_point(Term, Xn, Yn), K is K1+1); (K is K1)).

count_elem(X, Y, F, K):- count_elem_(X, Y, [arriba, abajo, izquierda, derecha, 
    arriba_izquierda, arriba_derecha, abajo_izquierda, abajo_derecha], F, K), !.

% rehace los puntos del tablero, una vez que ya se tenga las dimensiones del tablero lo 
% borra y lo rehace nuevamente, se le pasa la cantidad de elementos que se deseen poner en
% el tablero, tener presente que la cantidad de corrales es igual a la cantidad de ninos
% Boys:int, obligatory
% Dirties:int, obligatory
% Obj:int, obligatory
remake_points(Boys, Dirties, Obj):- clean_db([boy, 3, robot, 4, jail, 2, 
    dirty, 2, obj, 2]), (build_jail(Boys, Jail_list), 
    ran_uniq_disj_point(Jail_list, Obj, Obj_list, obj, 2), 
    append(Jail_list, Obj_list, Jails_Objs), 
    ran_uniq_disj_point(Jails_Objs, Boys, Boys_list, boy, 3),
    ran_uniq_disj_point(Jails_Objs, Dirties, Dirties_list, dirty, 2),
    append(Jails_Objs, Boys_list, Jails_Objs_Boys), 
    append(Jails_Objs_Boys, Dirties_list, Jails_Objs_Boys_Dirties),
    ran_uniq_disj_point(Jails_Objs_Boys_Dirties, 1, Robot_list, robot, 4),
    init_list_ag(Boys_list, 0), length(Boys_list, L), init_list_ag(Robot_list, L),
    init_list_robot(Robot_list), add_to_db(Jails_Objs_Boys_Dirties), 
    add_to_db(Robot_list)), !.


% similar al anterior pero en este caso solo para agentes, inicializa con el identificado el
% termino agente (nino o robot) que se le pase.
% Ag:term, obligatory
% Id:int
init_ag(Ag, Id):- arg(3, Ag, Id).


% inicializa con id la lista de agentes que se le pasan, el primero de la lista tendra el id
% K, el segundo el K+1, etc
init_list_ag([], _).
init_list_ag([X|R], K):- init_ag(X, K), K1 is K+1, init_list_ag(R, K1).

% inicializa un lista de robots, los robots que ya hallan sido inicializados como puntos y
% como agentes (ya tendran su X, Y e id), se les pondra en false la ultima columna, lo cual
% significa que no cargan ningun nino
% L:list, obligatory
init_list_robot([]).
init_list_robot([robot(_, _, _, false)|R]):- init_list_robot(R).

/* 
    agrega una lista de elementos a la base de datos de prolog
    L:list, obligatory
 */
add_to_db([]).
add_to_db([X|R]):- assertz(X), add_to_db(R).

% limpia la base de datos, elimina los terminos, se le pasa una lista de 
% functor, aridad, ..., y elimina los terminos con esa aridad.
% L:list, obligatory
clean_db([]).
clean_db([Func, Ar|R]):- functor(Term, Func, Ar), retractall(Term), clean_db(R).
