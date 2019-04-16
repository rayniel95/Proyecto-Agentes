:- module(utils, [disj/2]).

disj(Lista1, Lista2):- not((member(X, Lista1), member(X, Lista2))).

