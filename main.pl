
:-debug.
:-dynamic boy/3, robot/4, dirty/2, jail/2, obj/2.
% los predicados comentados ya han sido testeados.

% los predicados que terminan en _ se consideran 'privados', convencion usada para que solo
% puedan ser llamados (sobrecargados) con predicados de nombre igual, sin _, pero que
% necesiten menos argumentos

%%%%%%%%%%%%%%%%%%%%%%%%%%%% test %%%%%%%%%%%%%%%%%%%%%%%%%
robot(4, 7, _, _).
dirty(4, 7).
jail(4, 7).

obj(0, 0).
obj(2, 3).
obj(2, 4).
obj(2, 5).
obj(2, 6).
obj(2, 7).
obj(1, 5).
obj(3, 3).
obj(3, 4).
obj(3, 6).

boy(2, 2, _).
boy(1, 0, _).
boy(4, 2, _).
boy(4, 4, _).
boy(3, 5, _).
boy(3, 1, _).
%%%%%%%%%%%%%%%%%%%%%%%%%%%% test %%%%%%%%%%%%%%%%%%%%%%%%%

% tiempo en segundos, posteriromente hay q cambiarlo por algo que sea como una variable,
% usando modificacion de la bd
time(3).
% igual que el anterior
table_dim(5, 8).

% jail(X, Y).
% dirty(X, Y).
% boy(X, Y, id).
% robot(X, Y, id, carga).
% obj(X, Y).


% verifica si esta en la tabla las coordenadas X, Y
% X:int, obligatory
% Y:int, obligatory
in_table(X, Y):- table_dim(N, M), X<N, Y<M, 0=<X, 0=<Y.

% actualiza la base de datos de prolog, el termino T con los parametros O, lo elimima,
% y agrega el termino T con los parametros N.
% T:functor, obligatory
% O:list, obligatory
% N:list, obligatory
act(T, O, N):- C=..[T|O], C, retract(C), C1=..[T|N], assertz(C1), !.

% actualiza el objeto, que ya se encuentra en la base de datos, de la posicion X, Y
% hacia la Xn, Yn, puede ser visto como mover el objeto en el tablero
% X:int, obligatory
% Y:int, obligatory
% Xn:int, obligatory
% Yn:int, obligatory
act_obj(X, Y, Xn, Yn):- act(obj, [X, Y], [Xn, Yn]).

% actualiza el nino, que ya se encuentra en la base de datos, de la posicion X, Y
% hacia la Xn, Yn, puede ser visto como mover el nino en el tablero
% X:int, obligatory
% Y:int, obligatory
% Xn:int, obligatory
% Yn:int, obligatory
act_boy(X, Y, Xn, Yn):- act(boy, [X, Y, _], [Xn, Yn, _]).

% agrega una casilla sucia a la base de datos en la posicion X, Y del tablero
% X:int, obligatory
% Y:int, obligatory
dirt(X, Y):- assertz(dirty(X, Y)).

% elimina la casilla (las casillas) sucias de la base de datos, que se encuentra en la
% posicion X, Y del tabler. puede verse como limpiar esa parte del tablero
% X:int
% Y:int
clean(X, Y):- retract(dirty(X, Y)).

% adv: antes de usar estos predicados que hacen uso de terminos que deben estar en la base
% de datos, se deben definir tales terminos, o sea, estos deben de estar en la base de datos.
% devuelve si es valido poner un objeto en la posicion X, Y del tablero
% X:int, obligatory
% Y:int, obligatory
is_valid_for_obj(X, Y):- is_valid_for_boy(X, Y), not(dirty(X, Y)), not(jail(X, Y)).

% devuelve si es valido poner un nino en la posicion X, Y del tablero
% X:int, obligatory
% Y:int, obligatory
is_valid_for_boy(X, Y):- in_table(X, Y), not(boy(X, Y, _)), not(robot(X, Y, _, _)).

% devuelve si es valido poner una suciedad en la posicion X, Y del tablero
% X:int, obligatory
% Y:int, obligatory
is_valid_for_dirt(X, Y):- in_table(X, Y), not(dirty(X, Y)), not(obj(X, Y)), not(jail(X, Y)).

% dado un punto en el tablero devuelve el nuevo punto resultante de haber movido el punto
% inicial una direccion determinada
% X:int, obligatory
% Y:int, obligatory
% Xn:int
% Yn:int
mov_gradient(X, Y, Xn, Yn, arriba):- Xn is X-1, Yn is Y, !.
mov_gradient(X, Y, Xn, Yn, abajo):- Xn is X+1, Yn is Y, !.
mov_gradient(X, Y, Xn, Yn, izquierda):- Xn is X, Yn is Y-1, !.
mov_gradient(X, Y, Xn, Yn, derecha):- Xn is X, Yn is Y+1, !.
mov_gradient(X, Y, Xn, Yn, arriba_izquierda):- Xn is X-1, Yn is Y-1, !.
mov_gradient(X, Y, Xn, Yn, arriba_derecha):- Xn is X-1, Yn is Y+1, !.
mov_gradient(X, Y, Xn, Yn, abajo_izquierda):- Xn is X+1, Yn is Y-1, !.
mov_gradient(X, Y, Xn, Yn, abajo_derecha):- Xn is X+1, Yn is Y+1.

% triunfa si I es un item random de la lista L, tener cuidado, al ser un random a veces
% triunfa y a veces falla inclusi siendo I item valido en L
% L:list
select_ran_item(L, I):- length(L, Len), X is random(Len), nth0(X, L, I).


% mov_obj using logic operators
% mov_obj(X, Y, Dir):- mov_gradient(X, Y, Xn, Yn, Dir), is_valid_for_obj(Xn, Yn),
%     ((not(obj(Xn, Yn)), act_obj(X, Y, Xn, Yn));
%     (obj(Xn, Yn), not(mov_obj(Xn, Yn, Dir)), fail); act_obj(X, Y, Xn, Yn)).


% mov_obj using cutting
% mueve un objeto (obstaculo del tablero), que se encuentre en la posicion X, Y, en una
% direccion Dir. El movimiento se hace acorde a como un nino puede mover el obstaculo, o sea,
% se empuja. hay qie asegurarse que en la posicion X, Y exista de verdad un objeto, de no
% existir, empuja los demas objetos en esa direccion pero retorna false al no poder mover
% el inexistente de la X, Y.
% X:int, obligatory
% Y:int, obligatory
% Dir:term, obligatory
mov_obj(X, Y, Dir):- mov_gradient(X, Y, Xn, Yn, Dir), is_valid_for_obj(Xn, Yn),
    not(obj(Xn, Yn)), act_obj(X, Y, Xn, Yn), !.
mov_obj(X, Y, Dir):- mov_gradient(X, Y, Xn, Yn, Dir), is_valid_for_obj(Xn, Yn),
    mov_obj(Xn, Yn, Dir), act_obj(X, Y, Xn, Yn).

% predicado para mover un nino, se debe garantizar que halla un nino en la X, Y, acorde al
% documento
% X:int, obligatory
% Y:int, obligatory
mov_boy(X, Y):- select_ran_item([arriba, abajo, izquierda, derecha, arriba_izquierda,
    arriba_derecha, abajo_izquierda, abajo_derecha], Dir), mov_boy_(X, Y, Dir).

mov_boy_(X, Y, Dir):- mov_gradient(X, Y, Xn, Yn, Dir), is_valid_for_boy(Xn, Yn), obj(Xn, Yn),
    mov_obj(Xn, Yn, Dir), act_boy(X, Y, Xn, Yn), !.
mov_boy_(X, Y, Dir):- mov_gradient(X, Y, Xn, Yn, Dir), is_valid_for_boy(Xn, Yn),
    not(obj(Xn, Yn)), act_boy(X, Y, Xn, Yn).


% fail == false, no picha para hacer fallar toda la regla, buscar para en el caso de los
% predicados comentados alguna forma mas elegante logicamente (usando operadores) de hacer
% que funcionen sin que pase por el corte.


% mov_boy(X, Y, Dir):- mov_gradient(X, Y, Xn, Yn, Dir), !,
%     is_valid_for_boy(Xn, Yn),
%     ((obj(Xn, Yn), not(mov_obj(Xn, Yn, Dir)), fail); act_boy(X, Y, Xn, Yn)).


% dado un punto inicial P y las dimensiones de un tablero, devuelve todos aquellos puntos
% hacia donde es valido moverse, abajo, izquierda, derecha, arriba
% adv: notar como Xr y Yr ya son unificadas en uno de las formulas logicas del predicado
% y no hace falta forzar nuevamenta la unificacion puesto que se hace en mov_gradient
% pero si ese no fuera el caso se tendria que forzar la unificacion, si no, una vez
% unificadas tales variables ya no volverian a unificar con valores distintos
% (tips and tricks)
% Xp:int, obligatory
% Yp:int, obligatory
% Xr:int
% Yr:int
valid_point(Xp, Yp, Xr, Yr):- (mov_gradient(Xp, Yp, Xr, Yr, abajo), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, arriba), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, izquierda), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, derecha), in_table(Xr, Yr)).


% construye una lista de K corrales que forman una region conexa en el mapa, se selecciona un
% punto random
% P:jail
% K:int, obligatory
% L:list, obligatory
build_jail(jail(_, _), 0, []).
build_jail(jail(X0, Y0), 1, [jail(X0, Y0)]):- generate(X0, Y0), !.
build_jail(jail(X0, Y0), K, [jail(X0, Y0)|R]):- K1 is K-1, build_jail(jail(X, Y), K1, R),
    valid_point(X, Y, X0, Y0), not(member(jail(X0, Y0), R)).

% para considerar las clausulas como hechos, usar el corte, una vez evaluado, no volvera a
% evaluar otra clausula.

% usar terminos en la parte de los argumentos de los predicados, de esta forma se unifica
% directo, es como poner tipos en los predicados, los terminos son mis tipos, si pongo
% variable seria algo asi como un tipo generico.

% genera un punto aleatorio en el tablero
% Xp:int
% Yp:int
generate(Xp, Yp):- table_dim(N,M), Xp is random(N), Yp is random(M).

% inicializa un termino Term como un punto en el tablero, o sea, se supone que term sea un
% objeto del tablero que contenga como 2 primeros argumentos las coordenadas en el tablero.
% el predicado instancia tales coordenadas con los argumentos X, Y.
% Term:term, obligatory
% X:int
% Y:int
init_point(Term, X, Y):- arg(1, Term, X), arg(2, Term, Y).

% similar al anterior pero en este caso solo para agentes, inicializa con el identificado el
% termino agente (nino o robot) que se le pase.
% Ag:term, obligatory
% Id:int
init_ag(Ag, Id):- arg(3, Ag, Id).

% devuelve un termino, con functor F y aridad A, instanciado en un punto random del tablero
% F:functor, obligatory
% A:int, obligatory
% Term:term
make_ran_point(F, A, Term):- generate(Xp, Yp), functor(Term, F, A), init_point(Term, Xp, Yp).

% para usar un if, elif, else, usar clausulas con cortes al final, una vez que se ejecute
% una no volveran a ejecutarse las demas

% triunfa si el termino T, siendo considerado como un punto en el tablero, no se encuentra
% en la lista L
% L:list
% T:term
point_not_in([], T).
point_not_in([X|R], T):- arg(1, X, X0), arg(2, X, Y0), not((arg(1, T, X0), arg(2, T, Y0))),
    point_not_in(R, T).

% adv: a veces da error, overflow, o false, verificar pq. ****************funciona la mayoria
% las veces****************
% dada una lista de puntos L, devuelve una lista R, de longitud K, de terminos con functor F
% y aridad A, como puntos random en el tablero, de forma tal, que R y L son disjuntas, como
% puntos, y cada elemento de R es unico como punto.
% L:list, obligatory
% K:int, obligatory
% R:list
% T:functor, obligatory
% A:int, obligatory
ran_uniq_disj_point(L, 0, [], T, A).
ran_uniq_disj_point(L, 1, [P], T, A):- make_ran_point(T, A, P), point_not_in(L, P), !.
ran_uniq_disj_point(L, K, [P|R], T, A):- K1 is K-1, ran_uniq_disj_point(L, K1, R, T, A),
    make_ran_point(T, A, P), point_not_in(L, P), not(member(P, R)).

% agrega una lista de elementos a la base de datos de prolog
% L:list, obligatory
add_to_db([]).
add_to_db([X|R]):- assertz(X), add_to_db(R).


make_dirt_(X, Y, L, 0).
make_dirt_(X, Y, L, K):- select_ran_item(L, Dir), select(Dir, R, L),
    mov_gradient(X, Y, Xn, Yn, Dir), ((is_valid_for_dirt(Xn, Yn), dirt(Xn, Yn)); true),
    K1 is K-1, make_dirt_(X, Y, R, K1).


make_dirt(X, Y, K):- make_dirt_(X, Y, [arriba, abajo, izquierda, derecha, arriba_izquierda,
    arriba_derecha, abajo_izquierda, abajo_derecha], K).



count_elem_(X, Y, [], Term, 0).
count_elem_(X, Y, [Dir], Term, K):- K is 0, mov_gradient(X, Y, Xn, Yn, Dir),
    not(exist_point(Term, Xn, Yn)), !.
count_elem_(X, Y, [Dir], Term, K):- K is 1, mov_gradient(X, Y, Xn, Yn, Dir),
    exist_point(Term, Xn, Yn), !.    % si esta muy pesado solucionar con corte
count_elem_(X, Y, [Dir|R], Term, K):- count_elem_(X, Y, R, Term, K);
    (mov_gradient(X, Y, Xn, Yn, Dir), exist_point(Term, Xn, Yn), K is K+1).


count_elem(X, Y, F, K):- count_elem_(X, Y, [arriba, abajo, izquierda, derecha,
    arriba_izquierda, arriba_derecha, abajo_izquierda, abajo_derecha], F, K).


exist_point(Term, X, Y):- functor(Term, F, A), functor(Term2, F, A),
    init_point(Term2, X, Y), Term2.

% Para obtener una direccion aleatoria para hacer que el robot camine
% rand_direction(D):- select_ran_item([arriba, abajo, izquierda, derecha], D).


