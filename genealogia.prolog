% Ecrivons quelques faits.
fille(esther, albert, carole).
fille(esther, albert, sandra).
fille(esther, albert, nancy).
fille(sandra, jean-luc, romy).
fille(nancy, omar, elina).
fils(carole, michel, lucas).
fils(esther, albert, eliot).
fils(nancy, omar, thomas).

% Maintenant, réflechissons à quelques lois.
parents(X, Y, Z) :-
    fille(X, Y, Z) ;
    fils(X, Y, Z).

frere(A, B) :-
    dif(A, B),
    fils(X, Y, A),
    parents(X, Y, B).

soeur(A, B) :-
    dif(A, B),
    fille(X, Y, A),
    parents(X, Y, B).

oncle(A, B) :-
    parents(X, Y, A),
    (   frere(B, X)
    ;   frere(B, Y)
    ).

tante(A, B) :-
    parents(X, Y, A),
    (   soeur(B, X)
    ;   soeur(B, Y)
    ).

fos(A, B) :-
    frere(A, B) ;
    soeur(A, B).


cousin_m(A, B) :-
    parents(X, _, A),
    fos(X, X1),
    (   parents(X1, _, B)
    ;   parents(_, X1, B)
    ).

cousin_p(A, B) :-
    parents(_, Y, A),
    fos(Y, Y1),
    (   parents(Y1, _, B)
    ;   parents(_, Y1, B)
    ).

cousin(A, B) :-
    cousin_m(A, B) ;
    cousin_p(A, B).
