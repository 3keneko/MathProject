% Ecrivons quelques faits.
fils(louis_philippe_de_belgique ,louise_d_orleans, leopold1).
fils(leopold2, louise_d_orleans, leopold1).
fils(philippe_de_belgique, louise_d_orleans, leopold1).
fils(leopold_de_belgique, marie_henriette_d_autriche, leopold2).
fils(lucien_durrieux, blanche_delacroix, leopold2).
fils(philippe_durrieux, blanche_delacroix, leopold2).
fils(baudoin_de_belgique, marie_de_hohenzollern_sigmaringen,
     philippe_de_belgique).
fils(albert1, marie_de_hohenzollern_sigmaringen,
     philippe_de_belgique).
fils(charles_de_belgique, elisabeth_en_baviere, albert1).
fils(leopold3, elisabeth_en_baviere, albert1).
fils(baudoin1, astrid_de_suede, leopold3).
fils(albert2, astrid_de_suede, leopold3).
fils(alexandre_de_belgique, lilian_baels, leopold3).
fils(philippe1, paola, albert2).
fils(laurent, paola, albert2).
fils(gabriel, mathile, philippe1).
fils(emmanuel, mathilde, philippe1).
fils(amedeo, astrid, lorenz).
fils(joachim, astrid, lorenz).
fils(maximilian, elisabetta_rosboch, amedeo).
fils(nicolas, claire, laurent).
fils(aymeric, claire, laurent).

fille(charlotte_de_belgique, louise_d_orleans, leopold1).
fille(louise_de_belgique, marie_henriette_d_autriche, leopold2).
fille(stephanie_de_belgique, marie_henriette_d_autriche, leopold2).
fille(clementine_de_belgique, marie_henriette_de_belgique, leopold2).
fille(henriette_de_belgique, marie_de_hohenzollern_sigmaringen,
      philippe_de_belgique).
fille(josephine_de_belgique1, marie_de_hohenzollern_sigmaringen,
      philippe_de_belgique).
fille(josephine_de_belgique, marie_de_hohenzollern_sigmaringen,
      philippe_de_belgique).
fille(marie_jose_de_belgique, elisabeth_en_baviere,
      albert1).
fille(josephine_charlotte, astrid_de_suede, leopold3).
fille(marie_christine_de_belgique, lilian_baels, leopold3).
fille(maria_esmeralda_de_belgique, lilian_baels, leopold3).
fille(astrid, paola, albert2).
fille(elisabeth, mathilde, philippe1).
fille(eleonore, mathilde, philippe1).
fille(maria_laura, astrid, lorenz).
fille(luisa, astrid, lorenz).
fille(laetitia, astrid, lorenz).
fille(anna_astrid, elisabetta_rosboch, amedeo).
fille(louise, claire, laurent).

% Maintenant, réflechissons à quelques lois.
parents(Mere, Pere, Enfant) :-
    fille(Enfant, Mere, Pere) ;
    fils(Enfant, Mere, Pere).

grands_parents(GrandMere, GrandPere, PetitEnfant) :-
    parents(Mere, Pere, PetitEnfant),
    (   parents(GrandMere, GrandMere, Mere)
    ;   parents(GrandMere, GrandPere, Pere)
    ).

frere(Frere, FOS) :-
    dif(Frere, FOS), % FOS c'est pour Frère ou Soeur.
    fils(Frere, Mere, Pere),
    parents(Mere, Pere, FOS).

soeur(Soeur, FOS) :-
    dif(Soeur, FOS),
    fille(Soeur, Mere, Pere),
    parents(Mere, Pere, FOS).

oncle(Oncle, Neveu) :-
    parents(Mere, Pere, Neveu),
    (   frere(Oncle, Mere)
    ;   frere(Oncle, Pere)
    ).


tante(Tante, Neveu) :-
    parents(Mere, Pere, Neveu),
    (   soeur(Tante, Mere)
    ;   soeur(Tante, Pere)
    ).

fos(Enfant1, Enfant2) :-
    frere(Enfant1, Enfant2) ;
    soeur(Enfant1, Enfant2).


cousin_m(Cous1, Cousin) :-
    parents(Mere, _, Cous1),
    fos(Mere, Parent),
    (   parents(Parent, _, Cousin)
    ;   parents(_, Parent, Cousin)
    ).

cousin_p(Cous1, Cousin) :-
    parents(_, Pere, Cous1),
    fos(Pere, Parent),
    (   parents(Parent, _, Cousin)
    ;   parents(_, Parent, Cousin)
    ).

cousin(Cous1, Cousin) :-
    cousin_m(Cous1, Cousin) ;
    cousin_p(Cous1, Cousin).

descendant(Parent, Descendant) :-
    parents(Parent, _, Descendant) ;
    parents(_, Parent, Descendant).

descendant(Parent, Descendant) :-
    parents(X, Y, Descendant),
    (   descendant(Parent, X)
    ;   descendant(Parent, Y)
    ).
