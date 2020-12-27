:- use_module(library(clpfd)).

mathematicien(turing).

cool(X) :-
    mathematicien(X).

ajouter(A, B, C) :-
    C is A + B.

ajouter_clp(A, B, C) :-
    C #= A + B.
