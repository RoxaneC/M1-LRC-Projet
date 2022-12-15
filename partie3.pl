
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
							
%%%


% Tri de la A_Box étendue
tri_Abox([], Lie, Lpt, Li, Lu, Ls).
tri_Abox([ (I,some(R,C)) | Abi], [ (I,some(R,C)) | Lie], Lpt, Li, Lu, Ls) :-	tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls), !.
tri_Abox([ (I,all(R,C)) | Abi], Lie, [ (I,all(R,C)) | Lpt], Li, Lu, Ls) :-		tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls), !.
tri_Abox([ (I,and(C1,C2)) | Abi], Lie, Lpt, [ (I,and(C1,C2)) | Li], Lu, Ls) :-	tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls), !.
tri_Abox([ (I,or(C1,C2)) | Abi], Lie, Lpt, Li, [ (I,or(C1,C2)) | Lu], Ls) :-	tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls), !.
tri_Abox([ (I,C) | Abi], Lie, Lpt, Li, Lu, [ (I,C) | Ls]) :-					tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls), !.
tri_Abox([ (I,not(C)) | Abi], Lie, Lpt, Li, Lu, [ (I,not(C)) | Ls]) :-			tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls), !.


% Cas de "il existe"
complete_some([], Lpt, Li, Lu, Ls, Abr) :-	transformation_and([],Lpt,Li,Lu,Ls,Abr), !.
complete_some([ (I1,some(R,C)) | Lie], Lpt, Li, Lu, Ls, Abr) :-	genere(I2),

			evolue((I2,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
			
			resolution(Lie1, Lpt1, Li1, Lu1, Ls1, [ (I1, I2, R) | Abr]), !.


% Cas de "et"
transformation_and(Lie, Lpt, [], Lu, Ls, Abr) :-	deduction_all(Lie,Lpt,Li,Lu,Ls,Abr), !.
transformation_and(Lie, Lpt, [ (I,and(C1,C2)) | Li], Lu, Ls, Abr) :- 
			evolue([(I,C1), (I,C2)], Lie, Lpt, Li, Lu, Ls1, Lie1, Lpt1, Li1, Lu1, Ls1),
			
			resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr), !.


% Cas de "pour tout"
%% recursivité de member ? A revoir
deduction_all(Lie, [ (I1,all(R,C)) | Lpt], Li, Lu, Ls, Abr) :- 	
			member((I1,I2,R), Abr),
			
			evolue((I2,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
			
			resolution(Lie1, Lpt, Li1, Lu1, Ls1, Abr), !.


% Cas de "ou"
transformation_or(Lie, Lpt, Li, [ (I,or(C1,C2)) | Lu], Ls, Abr) :-	
			evolue((I,C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
			evolue((I,C2), Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
			
			resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr),
			resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr), !.


% Modification et mise à jour des listes
evolue([], Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls).
evolue([Elem | L], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 
			evolue(Elem, Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
			evolue(L, Lie2, Lpt2, Li2, Lu2, Ls2, Lie1, Lpt1, Li1, Lu1, Ls1), !.

evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt, Li, Lu, Ls) :- 	concat([ (I,some(R,C)) ], Lie, Lie1), !.
evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt1, Li, Lu, Ls) :- 	concat([ (I,all(R,C)) ], Lpt, Lpt1), !.
evolue((I,and(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li1, Lu, Ls) :- 	concat([ (I,and(C1,C2)) ], Li1, Li1), !.
evolue((I,or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu1, Ls) :- 	concat([ (I,or(C1,C2)) ], Lu, Lu1), !.
evolue((I,C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls1) :- 			concat([ (I,C) ], Ls, Ls1), !.
evolue((I,not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls1) :- 		concat([ (I,not(C)) ], Ls, Ls1), !.


% Affichage
affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2, Lpt2, Li2, Lu2, Abr2):-
	write('Etat Départ :'),nl,
	affiche(Ls1),
	affiche(Lie1),
	affiche(Lpt1),
	affiche(Li1),
	affiche(Lu1),
	affiche(Abr1),
	nl,
	write('Etat Arrivée :'),nl,
	affiche(Ls2),
	affiche(Lie2),
	affiche(Lpt2),
	affiche(Li2),
	affiche(Lu2),
	affiche(Abr2),
	nl.


% affichage
affiche((I,some(R,C))) :-	write(I), write(' : ∃ '),
							write(R), write('.'), write(C), !.
affiche((I,all(R,C))) :-	write(I), write(' : ∀ '),
							write(R), write('.'), write(C), !.
affiche((I,and(C1,C2))) :-	write(I), write(' : '),
							write(C1), write(' ⊓ '), write(C2), !.
affiche((I,or(C1,C2))) :-	write(I), write(' : '),
							write(C1), write(' ⊔ '), write(C2), !.
affiche((I,C)) :-	write(I), write(' : '), write(C1), !.
affiche((I,not(C))) :-	write(I), write(' : ¬'), write(C), !.

% récursivité
affiche([]) :-	nl.
affiche([Elem | L]) :-	affiche(Elem), write(', '),
						affiche(L), !.



%% A FAIRE

resolution(Lie, Lpt, Li, Lu, Ls, Abr) :- 	.

clash() :- .