
:-debug.
:-dynamic boy/3, robot/4, dirty/2, jail/2, obj/2, time_/1, dirties/1, objs/1, boys/1, 
    table_dim/2. 
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

table_dim(5, 8).

time_(3).

boys(6).
dirties(45).
objs(10).
%%%%%%%%%%%%%%%%%%%%%%%%%%%% test %%%%%%%%%%%%%%%%%%%%%%%%%

% jail(X, Y).
% dirty(X, Y).
% boy(X, Y, id).
% robot(X, Y, id, carga).
% obj(X, Y).


% verifica si esta en la tabla las coordenadas X, Y
% X:int, obligatory
% Y:int, obligatory
in_table(X, Y):- table_dim(N, M), NMinus1 is N-1, MMinus1 is M-1,
                 between(0, NMinus1, X), between(0, MMinus1, Y).

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
mov_boy(X, Y):- not(jail(X, Y)), select_ran_item([arriba, abajo, izquierda, derecha, 
    arriba_izquierda, arriba_derecha, abajo_izquierda, abajo_derecha], Dir), 
    mov_boy_(X, Y, Dir).

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
% K:int, obligatory
% L:list
build_jail(K, L):- build_jail_(_, K, L).

build_jail_(jail(_, _), 0, []).
build_jail_(jail(X0, Y0), 1, [jail(X0, Y0)]):- generate(X0, Y0), !.
build_jail_(jail(X0, Y0), K, [jail(X0, Y0)|R]):- K1 is K-1, build_jail_(jail(X, Y), K1, R), 
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
point_not_in([], _).
point_not_in([X|R], T):- arg(1, X, X0), arg(2, X, Y0), not((arg(1, T, X0), arg(2, T, Y0))),
    point_not_in(R, T).

% adv: a veces da error: overflow, o false, verificar pq. ****************funciona la mayoria 
% las veces****************
% dada una lista de puntos L, devuelve una lista R, de longitud K, de terminos con functor F
% y aridad A, como puntos random en el tablero, de forma tal, que R y L son disjuntas, como
% puntos, y cada elemento de R es unico como punto.
% L:list, obligatory
% K:int, obligatory
% R:list
% T:functor, obligatory
% A:int, obligatory
ran_uniq_disj_point(_, 0, [], _, _).
ran_uniq_disj_point(L, 1, [P], T, A):- make_ran_point(T, A, P), point_not_in(L, P), !.
ran_uniq_disj_point(L, K, [P|R], T, A):- K1 is K-1, ran_uniq_disj_point(L, K1, R, T, A), 
    make_ran_point(T, A, P), point_not_in(L, P), not(member(P, R)).

% agrega una lista de elementos a la base de datos de prolog
% L:list, obligatory
add_to_db([]).
add_to_db([X|R]):- assertz(X), add_to_db(R).


% se encarga de hacer la suciedad acorde al documento, selecciona random una de las 8 
% casillas adyacentes, si es valida para ensuciar se ensucia, si no, no se ensucia y vuelve
% repetir el mismo procedimiento, este se realiza K veces, con K casillas distintas
% X:int, obligatory
% Y:int, obligatory
% K:int, obligatory
% L:list
make_dirt_(_, _, _, 0).
make_dirt_(X, Y, L, K):- select_ran_item(L, Dir), select(Dir, R, L), 
    mov_gradient(X, Y, Xn, Yn, Dir), ((is_valid_for_dirt(Xn, Yn), dirt(Xn, Yn)); true), 
    K1 is K-1, make_dirt_(X, Y, R, K1).
% el que llama tiene que asegurarse de que el nino no se enctuentre en una casilla de corral
make_dirt(X, Y, K):- make_dirt_(X, Y, [arriba, abajo, izquierda, derecha, arriba_izquierda, 
    arriba_derecha, abajo_izquierda, abajo_derecha], K).


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


% dado un termino Term y un punto X, Y, dice si existe algun termino con functor igual a Term
% y que se encuentre en dicho punto.
% Term:term, obligatory
% X:int, obligatory
% Y:int, obligatory
exist_point(Term, X, Y):- functor(Term, F, A), functor(Term2, F, A), 
    init_point(Term2, X, Y), Term2.


% para que el ambiente sea factible es necesario crear un disjoint set o trabajar con un lee
% un dolor de cabeza no lo voy a implementar


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
    add_to_db(Jails_Objs_Boys_Dirties), add_to_db(Robot_list)).


clean_db([]).
clean_db([Func, Ar|R]):- functor(Term, Func, Ar), retractall(Term), clean_db(R).


make_env(N, M, Boys, Dirties, Obj):- assertz(table_dim(N, M)), 
    remake_points(Boys, Dirties, Obj).


perc_to_num(Total, Perc, Num):- Temp is Perc*Total, Num is Temp//100.
num_to_perc(Total, Perc, Num):- Temp is Num*100, Perc is Temp//Total.


is_dirty_house():- findall(dirty(X, Y), dirty(X, Y), R), length(R, Len), table_dim(N, M), 
    Temp is N*M, findall(jail(X, Y), jail(X, Y), Jail_list), length(Jail_list, Len_Jails),
    findall(obj(X, Y), obj(X, Y), Obj_list), length(Obj_list, Len_Objs), 
    Total is Temp-Len_Jails-Len_Objs, num_to_perc(Total, Perc, Len), 60=<Perc.


total_order():- not(dirty(_, _)), boy(X, Y, _), jail(X, Y).


init_list_ag([], _).
init_list_ag([X|R], K):- init_ag(X, K), K1 is K+1, init_list_ag(R, K1).


boys_actions([]).
boys_actions([boy(X, Y, _)|R]):- (boy(X, Y, _), not(jail(X, Y)), 
    count_elem(X, Y, boy(X, Y, _), K), ((K=<2, K1 is K); K1 is 6), make_dirt(X, Y, K1), 
    mov_boy(X, Y)); boys_actions(R).


ag_actions():- robot(_, _, Id, _), random_act_robot(Id), 
    findall(boy(X, Y, _), boy(X, Y, _), L), boys_actions(L).


% scheduler
run(T, R):- T1 is T+1, ag_actions(), (((is_dirty_house(); total_order()), R is T1, false); 
    (var(R), time(T2), T1=<(100*T2))), (((T1 mod T2)=:=0, boys(Boys), dirties(Dirties),
    objs(Objs), remake_points(Boys, Dirties, Objs)); true), run(T1, R).

% punto de entrada de la app, se le pasan los parametros y corre la simulacion
start(N, M, Boys, PercDirties, PercObj, Time):- clean_db([boy, 3, robot, 4, jail, 2, 
    dirty, 2, obj, 2, boys, 1, dirties, 1, objs, 1, table_dim, 2, time_, 1]), Total is N*M,
    perc_to_num(Total, PercDirties, NumDirties), perc_to_num(Total, PercObj, NumObjs), 
    assertz(time_(Time)), make_env(N, M, Boys, NumDirties, NumObjs), run(0, _).

% falta ir pintando los tableros, eso se haria en el scheduler



% Para obtener una direccion aleatoria para hacer que el robot camine
% rand_direction(D):- select_ran_item([arriba, abajo, izquierda, derecha], D).

init_robot(Id):- not(robot(_,_,Id,_)), make_ran_point(robot, 4, R), init_ag(R, Id), arg(4, R, false), add_to_db([R]).

% Obtiene una nueva coordenada posible para moverse teniendo como origen a X,Y
% X : obligatorio
% Y : obligatorio
% Xn: la nueva coordenada X
% Yn: la nueva coordenada Y
get_new_coord(X,Y,Xn,Yn):- findall([A,B], valid_point(X,Y,A,B), V), random_permutation(V, P), nth0(_, P, [Xn,Yn]).

% Un método valid point para cuando el robot tiene a un niño cargado
% Aqui solo se considera dar 1 o 2 pasos, si se dan 2 solo se dan en la misma dirrección que el anterior
% Funciona igual que el valid_point
valid_point_2(X,Y,Xn,Yn):-
    (mov_gradient(X, Y, Xn, Yn, arriba), in_table(Xn, Yn));
    (mov_gradient(X, Y, A, B, arriba), mov_gradient(A, B, Xn, Yn, arriba), in_table(Xn, Yn));
    (mov_gradient(X, Y, Xn, Yn, abajo), in_table(Xn, Yn));
    (mov_gradient(X, Y, A, B, abajo), mov_gradient(A, B, Xn, Yn, abajo), in_table(Xn, Yn));
    (mov_gradient(X, Y, Xn, Yn, izquierda), in_table(Xn, Yn));
    (mov_gradient(X, Y, A, B, izquierda), mov_gradient(A, B, Xn, Yn, izquierda), in_table(Xn, Yn));
    (mov_gradient(X, Y, Xn, Yn, derecha), in_table(Xn, Yn));
    (mov_gradient(X, Y, A, B, derecha), mov_gradient(A, B, Xn, Yn, derecha), in_table(Xn, Yn)).

separate_coord([X,Y],R):- findall([A,B], get_new_coord(X,Y,A,B), R).

plain_list([],[]).
plain_list([I],R):- append([],I,R).
plain_list([I|L],R):- plain_list(L,X), append(I,X,R).

get_2_steps_coord(X, Y, Xn, Yn):- findall([A,B], get_new_coord(X, Y, A, B), V),
    maplist(separate_coord, V, R), plain_list(R, Dirs),
    random_permutation(Dirs, Rand), nth0(_, Rand, [Xn,Yn]).

% Busca una casilla a la que el robot pueda moverse, da fail si no se puede mover a ninguna casilla.
% No retorna nada, solo mueve al robot si triunfa
% X : obligatorio. Posición X del robot
% Y : obligatorio. Posición Y del robot
% Id: obligatorio. identificador del robot
mov_rand_robot(X, Y, Id, Xn, Yn):- robot(X, Y, Id, ''), get_new_coord(X, Y, Xn, Yn), mov_bot(Id, X, Y, Xn, Yn), !.
mov_rand_robot(X, Y, Id, Xn, Yn):- robot(X, Y, Id, C), not(C = ''), get_2_steps_coord(X, Y, Xn, Yn), mov_bot(Id, X, Y, Xn, Yn), !.
mov_bot(Id, X, Y, Xn, Yn):- not(obj(Xn,Yn)), not(robot(Xn,Yn,_,_)), retract(robot(X,Y,Id,_)), assertz(robot(Xn,Yn,Id,_)), !.


% El robot que esté en la casilla (X, Y) recogerá al niño que esté en esa misma casilla.
% El niño dejará de estar localizable en la casilla (X, Y). Falla si falta alguna de estas condiciones
% X : obligatorio
% Y : obligatorio
pickup_boy(X, Y):- robot(X, Y, _, ''), boy(X, Y, BoyName), retract(boy(X, Y, BoyName)),
                   retract(robot(X,Y,Id,'')), assert(robot(X,Y,Id,BoyName)), !.

% El robot en la casilla (X, Y) dejará al niño que carga en la casilla (X,Y).
% Falla si falta alguna de las condiciones requeridas
% X : obligatorio
% Y : obligatorio
drop_boy(X, Y):- robot(X,Y,_,Boy), not(obj(X,Y)), not(boy(X, Y, _)), assert(boy(X,Y,Boy)),
                 retract(robot(X,Y,Id,Boy)), assert(robot(X,Y,Id,'')), !.

% Manda a actualizar el estado del robot que se comporta de manera "Random".
% X :
% Y :   Todos son obligatorios
% Id:
random_act_robot(Id):- robot(X,Y,Id,''), dirty(X,Y), clean(X,Y), !.
random_act_robot(Id):- robot(X,Y,Id,''), mov_rand_robot(X,Y,Id, Xn, Yn),
                           (not(jail(Xn,Yn)), pickup_boy(Xn,Yn); true), !.

random_act_robot(Id):- robot(X,Y,Id,_), jail(X,Y), not(boy(X,Y,_)), drop_boy(X,Y), !.
random_act_robot(Id):- robot(X,Y,Id,_), mov_rand_robot(X,Y,Id,_,_).

walk_to_nearest_boy(X,Y,Xn,Yn):- 
    findall([A,B], (boy(A,B,_), not(jail(A,B))), Boys), findall([C,D], obj(C,D), Obstacles), 
    bfs([[X,Y]],Boys,Obstacles,R), nth0(2, R, [Xn,Yn]).

% Sip, un bfs de toda la vida
bfs([[X,Y]|_],Targets,_,[X,Y]):- member([X,Y], Targets), !.
bfs([[X,Y]|R],Targets,Obstacles,L):- assert(visited(X,Y)), 
    findall([A,B], (valid_point(X,Y,A,B),not(member([A,B], Obstacles)), not(visited(A,B))), OtherPos),
    append(R, OtherPos, Next), bfs(Next, Targets, Obstacles, Z),!, append([[X,Y]], Z, L),
    retract(visited(X,Y)).

act_babysitter_robot(X,Y,Id,Xn,Yn):- 

% este movimiento está hecho para que el robot deje a los niños primero en una casilla de corral
% y después limpie la casa
% el resto de la lógica del método es igual que mov_rand_robot
mov_babysitter_robot(X,Y,Id,Xn,Yn):- robot(X,Y,Id,0), walk_to_nearest_boy(X,Y,Xn,Yn), (boy(Xn,Yn,Boy),
                                     mov_bot(Id,X,Y,Xn,Yn),
                                     not(jail(Xn,Yn)), pickup_boy(Xn,Yn); true).

% Creo que queda claro, imprime el mapa

print_map:- table_dim(F, C), write('  '), CMinus1 is C-1,FMinus1 is F-1,
            forall(between(0,CMinus1,Y), (write(Y), write(' '))), write_ln(""),
            forall(between(0, FMinus1, X), (write(X),write(' '), print_row(X, CMinus1), write_ln(""))).

print_row(X, C):- forall(between(0, C, Y), print_char(X,Y)).

print_char(X, Y):- robot(X,Y,_,_), !, write('R ').
print_char(X, Y):- obj(X,Y), !, write('O ').
print_char(X, Y):- boy(X,Y,_), !, write('N ').
print_char(X, Y):- dirty(X,Y), !, write('$ ').
print_char(X, Y):- jail(X,Y), !, write('C ').
print_char(_, _):- write('- ').

% Para probar funcionalidad del robot
test_robot(0):- print_map.
test_robot(X):- print_map, random_act_robot(yo), write_ln(""), 
                Y is X-1, test_robot(Y), !.
