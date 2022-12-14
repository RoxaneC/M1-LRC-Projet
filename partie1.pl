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


% Correction semantique ???

setof(X, cnamea(X), L).
setof(X, cnamena(X), L).
setof(X, iname(X), L).
setof(X, rname(X), L).


% Correction syntaxique
% On verifie que les concepts & les rôles en sont bien

concept(C) :- cnamea(C), !.
concept(C) :- cnamena(C), !.
concept(not(C)) :- concept(C), !.
concept(or(C1,C2)) :- concept(C1), concept(C2), !.
concept(and(C1,C2)) :- concept(C1), concept(C2), !.
concept(some(R,C)) :- role(R), concept(C), !.
concept(all(R,C)) :- role(R), concept(C), !.

instance(I) :- iname(I), !.		% verification
role(R) :- rname(R), !.


% On verifie qu'il n'y a pas de cycle
%% //?!?\\

autoref(C, C).
% autoref(C, D) :- autoref(C,D), developper(C,D), !.
autoref(C, and(A,B)) :-	autoref(C,A), autoref(C,B), !.
autoref(C, or(A,B)) :-	autoref(C,A), autoref(C,B), !.
autoref(C, some(R,B)) :-	autoref(C,B), !.
autoref(C, all(R,B)) :-	autoref(C,B), !.


% Traitements
%% Remplace les concepts non atomique en definition de concept atomique
remplace(CA, CA) :- cnamea(CA), !.
remplace(CNA, DCA) :- equiv(CNA, D), remplace(D, DCA) !.
remplace(not(CNA), not(CA)) :- 	remplace(CNA, CA), !.
remplace(or(CNA1, CNA2), or(CA1, CA2)) :- 	remplace(CNA1, CA1), remplace(CNA2, CA2), !.
remplace(and(CNA1, CNA2), and(CA1, CA2)) :- 	remplace(CNA1, CA1), remplace(CNA2, CA2), !.
remplace(some(R, CNA), some(R, CA)) :- 	remplace(CNA, CA), !.
remplace(all(R, CNA), all(R, CA)) :- 	remplace(CNA, CA), !.


traitement_Tbox([], []).
traitement_Tbox([(C,D) | Tbox], [(NC,ND) | L]) :- 	concept(C), concept(D),
													remplace(C,CA), remplace(D,DA),
													nnf(CA, NC), nnf(DA,ND),
													traitement_Tbox(Tbox, L), !.

traitement_AboxI([], []).
traitement_AboxI([(I,C) | Abox], [(I,NC) | L] ) :-	instance(I), concept(C),
													remplace(C,CA),
													nnf(CA, NC),
													traitement_AboxI(Abox, L), !.

traitement_AboxR([]).
traitement_AboxR([(I1,I2,R) | Abox]) :-	instance(I1), instance(I2), role(R),
										traitement_AboxR(Abox), !.

