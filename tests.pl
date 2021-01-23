use_module('main.pl').

% Para probar funcionalidad del robot
test_robot(0):- print_map.
test_robot(X):- print_map, act_robot(yo), write_ln(""),
                Y is X-1, test_robot(Y), !.

test_bfs(X,Y):- findall((A,B), dirty(A,B), Boys),
                findall((C,D), obj(C,D), Obs),
                assert(distance(X,Y,X,Y,0)),
                bfs([(0,X,Y)],Boys, Obs),
                retractall(distance(_,_,_,_,_)).
