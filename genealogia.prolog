% Ecrivons quelques faits.
fils(gaia, ouranos, coeos).
fils(gaia, ouranos, cronos).
fils(gaia, ouranos, oceaonos).
fils(gaia, ouranos, japet).
fils(rhea, cronos, hera).
fils(rhea, cronos, hades).
fils(rhea, cronos, poseidon).
fils(rhea, cronos, zeus).
fils(hera, zeus, ares).
fils(hera, zeus, hephaistos).
fils(aphrodite, ares, phobos).
fils(aphrodite, ares, deimos).
fils(clymene, japet, atlas).
fils(maia, zeus, hermes).
fils(leto, zeus, appolon).

fille(gaia, ouranos, rhea).
fille(gaia, ouranos, phebee).
fille(phebee, coeos, leto).
fille(phebee, coeos, asteria).
fille(tethys, oceanos, metis).
fille(tehtys, oceanos, dione).
fille(asteria, perses, hecate).
fille(rhea, cronos, hestia).
fille(rhea, cronos, demeter).
fille(rhea, cronos, hera).
fille(hera, zeus, hebe).
fille(hera, zeus, ilithye).
fille(metis, zeus, athena).
fille(demeter, zeus, persephone).
fille(aphrodite, ares, harmonie).
fille(pleionee, atlas, maia).
fille(leto, zeus, artemis).

% Maintenant, réflechissons à quelques lois.
parents(X, Y, Z) :-
    fille(X, Y, Z) ;
    fils(X, Y, Z).

grands_parents(X, Y, Z) :-
    parents(X1, Y1, Z),
    (   parents(X, Y, X1)
    ;   parents(X, Y, Y1)
    ).

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
