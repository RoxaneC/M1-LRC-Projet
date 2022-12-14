
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
nnf(all(R,C), all(R,NC)tri_Abox) :- nnf(C,NC), !.
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               %%
%%           Partie 2            %%
%%                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

programme :- 	premiere_etape(Tbox,Abi,Abr),
				deuxieme_etape(Abi,Abi1,Tbox),
				troisieme_etape(Abi1,Abr).

% 1.
% Création des listes codants les A-Box et T-Box

premiere_etape(Tbox,Abi,Abr):- 	setof((C, D), equiv(C, D), T), traitement_Tbox(T,Tbox),
								setof((I, C), inst(I, C), Ai), traitement_AboxI(Ai,Abi),
								setof((I1, I2, R), instR(I1, I2, R), Abr), traitement_AboxR(Abr), !.


% 2.
%% CODE DONNÉ
deuxieme_etape(Abi,Abi1,Tbox) :- 	saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
			nl, write('Entrez le numero du type de proposition que vous voulez demontrer :'), nl, 
			write('1 Une instance donnee appartient a un concept donne.'), nl,
			write('2 Deux concepts n"ont pas d"elements en commun(ils ont une intersection vide).'), nl, read(R), 
			suite(R,Abi,Abi1,Tbox).

suite(1,Abi,Abi1,Tbox) :- 	acquisition_prop_type1(Abi,Abi1,Tbox), !.
suite(2,Abi,Abi1,Tbox) :- 	acquisition_prop_type2(Abi,Abi1,Tbox), !.
suite(R,Abi,Abi1,Tbox) :- 	nl, write('Cette reponse est incorrecte.'), nl,
			saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).



%% 
% I : C  (ajout d'une instance)
acquisition_prop_type1(Abi,Abi1,Tbox) :- 
			nl, write('Entrez le nom de l"instance que vous souhaitez tester :'), nl,
			read(I), instance(I),
			nl, write('Entrez le concept associé que vous souhaitez tester :'), nl,
			read(C), concept(C),
			remplace(C, CA),
			nnf(not(CA), NCA),
			concat(Abi, [(I,NCA)], Abi1), !.


% C1 et C2

acquisition_prop_type2(Abi,Abi1,Tbox) :- 	
			nl, write('Entrez premier concept que vous souhaitez tester :'), nl,
			read(C1), concept(C1), remplace(C1, CA1),
			nl, write('Entrez le concept associé que vous souhaitez tester :'), nl,
			read(C2), concept(C2), remplace(C2, CA2),
			nnf(and(CA1, CA2), NCA),
			genere(Nom),
			concat(Abi, [(Nom, NCA)], Abi1), !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               %%
%%           Partie 3            %%
%%                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compteur(1).

%% CODE DONNÉ
troisieme_etape(Abi,Abr) :-	tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
							resolution(Lie,Lpt,Li,Lu,Ls,Abr), nl,
							write('Youpiiiiii, on a demontre la proposition initiale !!!').


% Tri de la A_Box étendue
tri_Abox([], [], [], [], [], []).
tri_Abox([ (I,some(R,C)) | Abi], [ (I,some(R,C)) | Lie], Lpt, Li, Lu, Ls) :-	tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,all(R,C)) | Abi], Lie, [ (I,all(R,C)) | Lpt], Li, Lu, Ls) :-		tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,and(C1,C2)) | Abi], Lie, Lpt, [ (I,and(C1,C2)) | Li], Lu, Ls) :-	tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,or(C1,C2)) | Abi], Lie, Lpt, Li, [ (I,or(C1,C2)) | Lu], Ls) :-	tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,C) | Abi], Lie, Lpt, Li, Lu, [ (I,C) | Ls]) :-					tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,not(C)) | Abi], Lie, Lpt, Li, Lu, [ (I,not(C)) | Ls]) :-			tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.



complete_some([ (I1,some(R,C)) | Lie], Lpt, Li, Lu, Ls, Abr) :-	genere(I2),
			evolue((I2,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
			
			resolution(Lie1, Lpt1, Li1, Lu1, Ls1, [ (I1, I2, R) | Abr]), !.



evolue([], Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls).
evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 	concat([ (I,some(R,C)) ], Lie, Lie1), !.
evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 	concat([ (I,all(R,C)) ], Lpt, Lpt1), !.
evolue((I,and(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 	concat([ (I,and(C1,C2)) ], Li1, Li1), !.
evolue((I,or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 	concat([ (I,or(C1,C2)) ], Lu, Lu1), !.
evolue((I,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 			concat([ (I,C) ], Ls, Ls1), !.
evolue((I,not(C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 		concat([ (I,not(C)) ], Ls, Ls1), !.

%% A FAIRE

resolution(Lie, Lpt, Li, Lu, Ls, Abr) :- 	.

transformation_and(Lie,Lpt,Li,Lu,Ls,Abr) :- 	.

deduction_all(Lie,Lpt,Li,Lu,Ls,Abr) :- 	.

transformation_or(Lie,Lpt,Li,Lu,Ls,Abr) :- 	.


affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2, Lpt2, Li2, Lu2, Abr2) :- .











