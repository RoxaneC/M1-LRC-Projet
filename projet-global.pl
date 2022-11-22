
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               %%
%%         Préliminaires         %%
%%                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Définiton des prédicats de mise sous forme normale négative


nnf(not(and(C1,C2)), or(NC1,NC2)) :- nnf(not(C1),NC1), nnf(not(C2),NC2), !.
nnf(not(or(C1,C2)), and(NC1,NC2)) :- nnf(not(C1),NC1), nnf(not(C2),NC2), !.
nnf(not(all(R,C)), some(R,NC)) :- nnf(not(C),NC), !.
nnf(not(some(R,C)), all(R,NC)) :- nnf(not(C),NC), !.
nnf(not(not(X)), X) :- !.
nnf(not(X), not(X)) :- !.
nnf(and(C1,C2), and(NC1,NC2)) :- nnf(C1,NC1), nnf(C2,NC2), !.
nnf(or(C1,C2), or(NC1,NC2)) :- nnf(C1,NC1), nnf(C2,NC2), !.
nnf(some(R,C), some(R,NC)) :- nnf(C,NC), !.
nnf(all(R,C), all(R,NC)) :- nnf(C,NC), !.
nnf(X,X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               %%
%%           Partie 1            %%
%%                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Définit les identificateurs


cnamea(personne).
cnamea(livre).
cnamea(objet).
cnamea(sculpture).
cnamea(anything).
cnamea(nothing).

cnamena(auteur).
cnamena(editeur).
cnamena(sculpteur).
cnamena(parent).

iname(michelAnge).
iname(david).
iname(sonnets).
iname(vinci).
iname(joconde).

rname(aCree).
rname(aEcrit).
rname(aEdite).
rname(aEnfant).


% T-Box

equiv(sculpteur,and(personne,some(aCree,sculpture))).
equiv(auteur,and(personne,some(aEcrit,livre))).
equiv(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite,livre)))).
equiv(parent,and(personne,some(aEnfant,anything))).

% Liste de doublets codant la T-Box
%
% [(sculpteur,and(personne,some(aCree,sculpture))), (auteur,and(personne,some(aEcrit,livre))),(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite,livre)))),(parent,and(personne,some(aEnfant,anything)))]


% A-Box

inst(michelAnge,personne).
inst(david,sculpture).
inst(sonnets,livre).
inst(vinci,personne).
inst(joconde,objet).

instR(michelAnge, david, aCree).
instR(michelAnge, sonnets, aEcrit).
instR(vinci, joconde, aCree).

% Liste de n-uplets codant les assertions de concepts (A-Box) :
%
% [(michelAnge,personne), (david,sculpture), (sonnets,livre),(vinci,personne), (joconde,objet)]
%
% Liste de n-uplets codant les assertions de rôles (A-Box) :
%
% [(michelAnge, david, aCree), (michelAnge, sonnet, aEcrit),(vinci,joconde, aCree)]


% Correction sémentique

setof(X, cnamea(X), L).
setof(X, cnamena(X), L).
setof(X, iname(X), L).
setof(X, rname(X), L).


% Correction syntaxique
% On verifie que les concepts en sont bien

concept(C) :- cnamea(C), !.
concept(C) :- cnamena(C), !.
concept(not(C)) :- concept(C), !.
concept(or(C1,C2)) :- concept(C1), concept(C2), !.
concept(and(C1,C2)) :- concept(C1), concept(C2), !.
concept(some(R,C)) :- rname(R), concept(C), !.
concept(all(R,C)) :- rname(R), concept(C), !.












