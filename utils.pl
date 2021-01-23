:- dynamic dirty/2, jail/2, boy/3, robot/4, table_dim/2.


% verifica si esta en la tabla las coordenadas X, Y
% X:int, obligatory
% Y:int, obligatory
% in_table(X, Y):- table_dim(N, M), X<N, Y<M, 0=<X, 0=<Y.
in_table(X, Y):- table_dim(N, M), NMinus1 is N-1, MMinus1 is M-1,
                 between(0, NMinus1, X), between(0, MMinus1, Y).


% agrega una casilla sucia a la base de datos en la posicion X, Y del tablero
% X:int, obligatory
% Y:int, obligatory
dirt(X, Y):- assertz(dirty(X, Y)).

% elimina la casilla (las casillas) sucias de la base de datos, que se encuentra en la 
% posicion X, Y del tabler. puede verse como limpiar esa parte del tablero
% X:int
% Y:int
clean(X, Y):- retract(dirty(X, Y)).

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

% devuelve un termino, con functor F y aridad A, instanciado en un punto random del tablero 
% F:functor, obligatory
% A:int, obligatory
% Term:term
make_ran_point(F, A, Term):- generate(Xp, Yp), functor(Term, F, A), init_point(Term, Xp, Yp).

make_ran_point_(F, A, _):- findall(T, (functor(T, F, A), in_table(_, _), point_not_in()), _).

to_list_point([], []).
to_list_point([T|R1], [[X, Y]|R]):- arg(1, T, X), arg(2, T, Y), to_list_point(R1, R).

select_ran_point(L, P):- findall([X, Y], in_table(X, Y), L1), 
    findall([X, Y], (member([X, Y], L1), not(member([X, Y], L))), R), random_member(P, R).

% dado un termino Term y un punto X, Y, dice si existe algun termino con functor igual a Term
% y que se encuentre en dicho punto.
% Term:term, obligatory
% X:int, obligatory
% Y:int, obligatory
exist_point(Term, X, Y):- functor(Term, F, A), functor(Term2, F, A), 
    init_point(Term2, X, Y), Term2.

% como su propio nombre lo indica, transforma de porciento a numero y a la inversa
perc_to_num(Total, Perc, Num):- Temp is Perc*Total, Num is Temp//100.
num_to_perc(Total, Perc, Num):- Temp is Num*100, Perc is Temp//Total.

% Obtiene una nueva coordenada posible para moverse teniendo como origen a X,Y
% X : obligatorio
% Y : obligatorio
% Xn: la nueva coordenada X
% Yn: la nueva coordenada Y
get_new_coord(X,Y,Xn,Yn):- findall([A,B], valid_point(X,Y,A,B), V), random_permutation(V, P), nth0(_, P, [Xn,Yn]).
get_new_coord_2(X,Y,Xn,Yn):- findall([A,B], valid_point_2(X,Y,A,B), V), random_permutation(V, P), nth0(_, P, [Xn,Yn]).

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

get_2_steps_coord(X, Y, Xn, Yn):- findall([A,B], get_new_coord_2(X, Y, A, B), V),
    maplist(separate_coord, V, R), plain_list(R, Dirs),
    random_permutation(Dirs, Rand), nth0(_, Rand, [Xn,Yn]).

% Creo que queda claro, imprime el mapa
print_map():- table_dim(F, C), write('  '), CMinus1 is C-1,FMinus1 is F-1,
            forall(between(0,CMinus1,Y), (write(Y), write(' '))), write_ln(""),
            forall(between(0, FMinus1, X), (write(X),write(' '), print_row(X, CMinus1), write_ln(""))).

print_row(X, C):- forall(between(0, C, Y), print_char(X,Y)).

print_char(X, Y):- robot(X,Y,_,_), !, write('R ').
print_char(X, Y):- obj(X,Y), !, write('O ').
print_char(X, Y):- boy(X,Y,_), !, write('N ').
print_char(X, Y):- dirty(X,Y), !, write('$ ').
print_char(X, Y):- jail(X,Y), !, write('C ').
print_char(_, _):- write('- ').

valid_agent_point(Xp,Yp,Xr,Yr):- (mov_gradient(Xp, Yp, Xr, Yr, abajo), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, arriba), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, izquierda), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, derecha), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, abajo_izquierda), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, abajo_derecha), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, arriba_izquierda), in_table(Xr, Yr));
    (mov_gradient(Xp, Yp, Xr, Yr, arriba_derecha), in_table(Xr, Yr)).


