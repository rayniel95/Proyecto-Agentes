use_module('utils.pl').
use_module('environment.pl').

:-debug.
:-dynamic boy/3, robot/4, dirty/2, jail/2, obj/2, time_/1, dirties/1, objs/1, table_dim/2, 
    boys/1. 
:-prolog_load_context(script, true).
% los predicados comentados ya han sido testeados.

% los predicados que terminan en _ se consideran 'privados', convencion usada para que solo
% puedan ser llamados (sobrecargados) con predicados de nombre igual, sin _, pero que 
% necesiten menos argumentos

% jail(X, Y).
% dirty(X, Y).
% boy(X, Y, id).
% robot(X, Y, id, carga).
% obj(X, Y).


% actualiza la base de datos de prolog, el termino T con los parametros O, lo elimima,
% y agrega el termino T con los parametros N.
% T: functor, obligatory
% O: list, obligatory
% N: list, obligatory
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

% adv: antes de usar estos predicados que hacen uso de terminos que deben estar en la base
% de datos, se deben definir tales terminos, o sea, estos deben de estar en la base de datos.


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

% predicado para mover un nino, se debe garantizar que haya un nino en la X, Y, acorde al 
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


% para considerar las clausulas como hechos, usar el corte, una vez evaluado, no volvera a 
% evaluar otra clausula.

% usar terminos en la parte de los argumentos de los predicados, de esta forma se unifica
% directo, es como poner tipos en los predicados, los terminos son mis tipos, si pongo 
% variable seria algo asi como un tipo generico.

% crea el mundo dadas las dimensiones y las cantidades casillas con ninos, suciedad y objetos
% N:int, obligatory
% M:int, obligatory
% Boys:int, obligatory
% Dirties:int, obligatory
% Objs:int, obligatory
make_env(N, M, Boys, Dirties, Obj):- retractall(table_dim(_, _)), assert(table_dim(N, M)), 
    remake_points(Boys, Dirties, Obj).


% verifica que el 60% de las casillas que se pueden ensuciar (que no sean obstaculos ni 
% corral), esten sucias. si hay mas de un table_dim unificara varias veces.
is_dirty_house():- findall(dirty(X, Y), dirty(X, Y), R), length(R, Len), table_dim(N, M), 
    Temp is N*M, findall(jail(X, Y), jail(X, Y), Jail_list), length(Jail_list, Len_Jails),
    findall(obj(X, Y), obj(X, Y), Obj_list), length(Obj_list, Len_Objs), 
    Total is Temp-Len_Jails-Len_Objs, num_to_perc(Total, Perc, Len), 60=<Perc.


% devuelve true o false si se ha alcanzado el estado final, de todo limpio y todos los ninos
% en el corral
total_order():- not(dirty(_, _)), findall(boy(X, Y, _), boy(X, Y, _), L), 
    forall(member(boy(X, Y, _), L), jail(X, Y)), true.


% funciona perfectamente, solo hay que ponerle un corte cuando se llame pq si no sigue 
% unificando, hace que los ninos en la lista realicen las acciones, sigue unificando K y 
% make_dirt, hay que poner varios cortes, o usar un findall cuando se llame
boys_actions([]). % tambien se puede separar en dos cortes para hacerlo mas falcil
boys_actions([boy(X, Y, _)|R]):- (boy(X, Y, _), not(jail(X, Y)), 
    count_elem(X, Y, boy(X, Y, _), K), ((K=<2, K1 is K); K1 is 6), make_dirt(X, Y, K1),
    mov_boy(X, Y)); boys_actions(R).
% incluso se puede poner un corte despues de K para que no vuelva a unificar

% mueve tanto el robot como los ninos en la lista
ag_actions():- (robot(_, _, Id, _), act_robot(Id); true),
    findall(boy(X, Y, _), boy(X, Y, _), L), findall(_, boys_actions(L), _).


% se puede separar usando cortes y quitando los operadores logicos para hacerlo mas legible
% y menos proclive a errores
% scheduler
run(T, T):- true.%, write_ln('Timeout!!!'), !.
run(T, CT):- T1 is T+1, ag_actions(), %print_map(), write_ln(""),
    (((is_dirty_house(); total_order()), !, false); true), ((time_(T2), 
    (T1 mod T2)=:=0, boys(Boys), dirties(Dirties), objs(Objs),% write_ln("new env"), 
    remake_points(Boys, Dirties, Objs)); true), run(T1, CT).

% run(T, R):- T1 is T+1, ag_actions(), (((is_dirty_house(); total_order()), R is T1, false); 
%     (var(R), time(T2), T1=<(100*T2))), (((T1 mod T2)=:=0, boys(Boys), dirties(Dirties),
%     objs(Objs), remake_points(Boys, Dirties, Objs)); true), run(T1, R).

% Para simular varias veces una misma estrategia con parámetros fijos

simulate(0,_,_,_,_,_,_,_,DirtList):- average(DirtList, Ave), write('DirtAverage:'), write(Ave).
simulate(Simulations,N,M,Boys,Dirties,Objs,TotalTime,ChangeTime,DirtList):- write(Simulations), write('- '),
    (start(N,M,Boys,Dirties,Objs,TotalTime,ChangeTime);true),
    evaluate(Dirt),
    append([Dirt],DirtList,MoreDirt),
    S is Simulations-1,
    simulate(S,N,M,Boys,Dirties,Objs,TotalTime,ChangeTime,MoreDirt), !.

average(L,A):- sumlist(L, N), length(L, M), A is N/M.

evaluate(Frac):- not(dirty(_,_)), write_ln('Clean! Robot Wins!!!'), Frac is 0.
evaluate(Frac):- findall((X,Y), dirty(X,Y), Bag), length(Bag, L), L > 0, !,
    (is_dirty_house(), write('Not clean. Too dirty.'); write('Not clean.')),
    table_dim(A,B), Frac is (L/(A*B))*100, write(Frac), write_ln('% dirty.').

% punto de entrada de la app, se le pasan los parametros y corre la simulacion
start(N, M, Boys, PercDirties, PercObj, Time, ChangeTime):- clean_db([boy, 3, robot, 4, jail, 2, 
    dirty, 2, obj, 2, boys, 1, dirties, 1, objs, 1, table_dim, 2, time_, 1]), Total is N*M,
    perc_to_num(Total, PercDirties, NumDirties), perc_to_num(Total, PercObj, NumObjs), 
    assertz(time_(Time)), assertz(boys(Boys)), assertz(dirties(NumDirties)), 
    assertz(objs(NumObjs)), make_env(N, M, Boys, NumDirties, NumObjs), run(0, ChangeTime), !.

% Para obtener una direccion aleatoria para hacer que el robot camine
% rand_direction(D):- select_ran_item([arriba, abajo, izquierda, derecha], D).

init_robot(Id):- not(robot(_,_,Id,_)), make_ran_point(robot, 4, R), init_ag(R, Id), arg(4, R, false), add_to_db([R]).


% Busca una casilla a la que el robot pueda moverse, da fail si no se puede mover a ninguna casilla.
% No retorna nada, solo mueve al robot si triunfa
% X : obligatorio. Posición X del robot
% Y : obligatorio. Posición Y del robot
% Id: obligatorio. identificador del robot
mov_rand_robot(X, Y, Id, Xn, Yn):- robot(X, Y, Id, false), get_new_coord(X, Y, Xn, Yn), mov_bot(Id, X, Y, Xn, Yn), !.
mov_rand_robot(X, Y, Id, Xn, Yn):- robot(X, Y, Id, C), not(C = ''), get_2_steps_coord(X, Y, Xn, Yn), mov_bot(Id, X, Y, Xn, Yn), !.
mov_bot(Id, X, Y, Xn, Yn):- not(obj(Xn,Yn)), not(robot(Xn,Yn,_,_)), retract(robot(X,Y,Id,Boy)), assertz(robot(Xn,Yn,Id,Boy)), !.


% El robot que esté en la casilla (X, Y) recogerá al niño que esté en esa misma casilla.
% El niño dejará de estar localizable en la casilla (X, Y). Falla si falta alguna de estas condiciones
% X : obligatorio
% Y : obligatorio
pickup_boy(X, Y):- robot(X, Y, _, false), boy(X, Y, BoyName), retract(boy(X, Y, BoyName)),
                   retract(robot(X,Y,Id,false)), assert(robot(X,Y,Id,BoyName)), !.

% El robot en la casilla (X, Y) dejará al niño que carga en la casilla (X,Y).
% Falla si falta alguna de las condiciones requeridas
% X : obligatorio
% Y : obligatorio
drop_boy(X, Y):- robot(X,Y,_,Boy), not(obj(X,Y)), not(boy(X, Y, _)), number(Boy), assert(boy(X,Y,Boy)),
                 retract(robot(X,Y,Id,Boy)), assert(robot(X,Y,Id,false)), !.

% Manda a actualizar el estado del robot que se comporta de manera "Random".
% Id: Obligatorio
% act_robot(Id):- robot(X,Y,Id,false), dirty(X,Y), clean(X,Y), !.
% act_robot(Id):- robot(X,Y,Id,false), mov_rand_robot(X,Y,Id, Xn, Yn),
%                            (not(jail(Xn,Yn)), pickup_boy(Xn,Yn); true), !.
% act_robot(Id):- robot(X,Y,Id,_), jail(X,Y), (drop_boy(X,Y); true), !.
% act_robot(Id):- robot(X,Y,Id,_), mov_rand_robot(X,Y,Id,_,_).

% Manda a actualizar el estado del robot que recoge primero a los niños y después limpia la casa.
% Id: Obligatorio
% act_robot(Id):- robot(X,Y,Id,false), dirty(X,Y), clean(X,Y), !.
% act_robot(Id):- robot(X,Y,Id,false), mov_babysitter_robot(X,Y,Id, Xn, Yn),
%                            (not(jail(Xn,Yn)), pickup_boy(Xn,Yn); true), !.
% act_robot(Id):- robot(X,Y,Id,Boy), jail(X,Y), not(boy(X,Y,_)), number(Boy), (drop_boy(X,Y); true), !.
% act_robot(Id):- robot(X,Y,Id,_), mov_babysitter_robot(X,Y,Id,_,_).

% Manda a actualizar el estado del robot que primero limpia la casa, solo limpia, los ninos
% van solos para el corral
act_robot(Id):- robot(X,Y,Id,false), dirty(X,Y), clean(X,Y), !.
act_robot(Id):- robot(X,Y,Id,false), mov_vacuum_bot(X,Y,Id, Xn, Yn),
                           (not(jail(Xn,Yn)), pickup_boy(Xn,Yn); true), !.
act_robot(Id):- robot(X,Y,Id,Boy), jail(X,Y), not(boy(X,Y,_)), number(Boy), (drop_boy(X,Y); true), !.
act_robot(Id):- robot(X,Y,Id,_), mov_vacuum_bot(X,Y,Id,_,_).

walk_to_nearest_boy(X,Y,Xn,Yn):-
    findall((A,B), (boy(A,B,_), not(jail(A,B))), Boys), findall((C,D), obj(C,D), Obstacles),
    assert(distance(X,Y,X,Y,0)),
    bfs([(0,X,Y)],Boys,Obstacles), 
    distance(Xd,Yd,_,_,_), boy(Xd,Yd,_), not(jail(Xd,Yd)),
    get_next_step(X,Y,Xd,Yd,Xn,Yn),
    retractall(distance(_,_,_,_,_)).

walk_to_nearest_jail(X,Y,Xn,Yn):-
    findall((A,B), (jail(A,B), not(boy(A,B,_))), Jails), findall((C,D), obj(C,D), Obstacles),
    assert(distance(X,Y,X,Y,0)),
    bfs([(0,X,Y)], Jails, Obstacles),
    distance(Xd,Yd,_,_,_), jail(Xd,Yd), not(boy(Xd,Yd,_)),
    get_next_step2(X,Y,Xd,Yd,Xn,Yn),
    retractall(distance(_,_,_,_,_)).

walk_to_nearest_dirt(X,Y,Xn,Yn):-
    findall((A,B), dirty(A,B), Dirts), findall((C,D), obj(C,D), Obstacles),
    assert(distance(X,Y,X,Y,0)),
    bfs([(0,X,Y)], Dirts, Obstacles),
    distance(Xd,Yd,_,_,_), dirty(Xd,Yd),
    get_next_step(X,Y,Xd,Yd,Xn,Yn),
    retractall(distance(_,_,_,_,_)).


get_next_step(X,Y,Xd,Yd,Xn,Yn):- distance(Xd,Yd,X,Y,D), D>0, Xn is Xd, Yn is Yd. 
get_next_step(X,Y,Xd,Yd,Xn,Yn):- distance(Xd,Yd,Xp,Yp,D), D>0, get_next_step(X,Y,Xp,Yp,Xn,Yn).

get_next_step2(X,Y,Xd,Yd,Xn,Yn):- (distance(Xd,Yd,_,_,2);distance(_,_,Xd,Yd,2); 
                                   distance(Xd,Yd,X,Y,1);distance(X,Y,Xd,Yd,1)), Xn is Xd, Yn is Yd. 

get_next_step2(X,Y,Xd,Yd,Xn,Yn):- (distance(Xd,Yd,Xp,Yp,D);distance(Xp,Yp,Xd,Yd,D)), D>0, 
                                  (distance(Xp,Yp,Xd,Yd,_);distance(Xd,Yd,Xp,Yp,_)),
                                  get_next_step2(X,Y,Xp,Yp,Xn,Yn).

% Sip, un bfs de toda la vida
bfs([], _, _):- !, fail.
bfs(L,Targets,_):-
    member((_,X,Y),L) , member((X,Y), Targets),!.
    
bfs([(D,X,Y)|R],Targets,Obstacles):-
    findall((A,B), (valid_agent_point(X,Y,A,B),not(member((A,B), Obstacles))), OtherPos),
    findall((M,N),(member((M,N), OtherPos), not(distance(M,N,_,_,_))), NextPos),
    Dn is D+1,
    mark_distance(NextPos,Dn,(X,Y), Marked),
    append(R,Marked,Z), bfs(Z, Targets, Obstacles),!.

mark_distance([],_,_,[]).
mark_distance([(X,Y)|R1],Dist,(Xn,Yn),[(Dist,X,Y)|R2]):- assert(distance(X,Y,Xn,Yn,Dist)),
    assert(distance(Xn,Yn,X,Y,Dist)), mark_distance(R1,Dist,(Xn,Yn),R2).

% este movimiento está hecho para que el robot deje a los niños primero en una casilla de corral
% y después limpie la casa
% el resto de la lógica del método es igual que mov_rand_robot
mov_babysitter_robot(X,Y,Id,Xn,Yn):- robot(X,Y,Id,false), boy(A,B,_), not(jail(A,B)), !,
                                     walk_to_nearest_boy(X,Y,Xn,Yn), mov_bot(Id,X,Y,Xn,Yn).
mov_babysitter_robot(X,Y,Id,Xn,Yn):- robot(X,Y,Id,false), dirty(_,_), walk_to_nearest_dirt(X,Y,Xn,Yn),
                                     mov_bot(Id,X,Y,Xn,Yn).
mov_babysitter_robot(X,Y,Id,Xn,Yn):- robot(X,Y,Id,_), walk_to_nearest_jail(X,Y,Xn,Yn),
                                     mov_bot(Id,X,Y,Xn,Yn).
mov_babysitter_robot(X,Y,_,X,Y):- dirty(X,Y), clean(X,Y).

% este movimiento está hecho para que el robot limpie siempre el churre más cercano
mov_vacuum_bot(X,Y,_,_,_):- dirty(X,Y), clean(X,Y).
mov_vacuum_bot(X,Y,Id,Xn,Yn):- robot(X,Y,Id,false), dirty(_,_), not(dirty(X,Y)), walk_to_nearest_dirt(X,Y,Xn,Yn),
                               mov_bot(Id,X,Y,Xn,Yn).
mov_vacuum_bot(X,Y,Id,X,Y):- robot(X,Y,Id,_), drop_boy(X,Y).
