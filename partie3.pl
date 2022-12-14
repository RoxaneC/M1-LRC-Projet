
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
tri_Abox([], [], [], [], [], []).
tri_Abox([ (I,some(R,C)) | Abi], [ (I,some(R,C)) | Lie], Lpt, Li, Lu, Ls) :-	tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,all(R,C)) | Abi], Lie, [ (I,all(R,C)) | Lpt], Li, Lu, Ls) :-		tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,and(C1,C2)) | Abi], Lie, Lpt, [ (I,and(C1,C2)) | Li], Lu, Ls) :-	tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,or(C1,C2)) | Abi], Lie, Lpt, Li, [ (I,or(C1,C2)) | Lu], Ls) :-	tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,C) | Abi], Lie, Lpt, Li, Lu, [ (I,C) | Ls]) :-					tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.
tri_Abox([ (I,not(C)) | Abi], Lie, Lpt, Li, Lu, [ (I,not(C)) | Ls]) :-			tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls), !.


% Cas de "il existe"
complete_some([], Lpt, Li, Lu, Ls, Abr) :-	transformation_and([],Lpt,Li,Lu,Ls,Abr), !.
complete_some([ (I1,some(R,C)) | Lie], Lpt, Li, Lu, Ls, Abr) :-	genere(I2),

			evolue((I2,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
			
			complete_some(Lie, Lpt1, Li1, Lu1, Ls1, [ (I1, I2, R) | Abr]), !.


% Cas de "et"
transformation_and(Lie, Lpt, [], Lu, Ls, Abr) :-	deduction_all(Lie,Lpt,Li,Lu,Ls,Abr), !.
transformation_and(Lie, Lpt, [ (I,and(C1,C2)) | Li], Lu, Ls, Abr) :- 
			evolue((I,C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
			evolue((I,C2), Lie1, Lpt1, Li1, Lu1, Ls1, Lie2, Lpt2, Li2, Lu2, Ls2),
			
			transformation_and(Lie2, Lpt2, Li, Lu2, Ls2, Abr), !.


% Modification 
evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 	concat([ (I,some(R,C)) ], Lie, Lie1), !.
evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 	concat([ (I,all(R,C)) ], Lpt, Lpt1), !.
evolue((I,and(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 	concat([ (I,and(C1,C2)) ], Li1, Li1), !.
evolue((I,or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 	concat([ (I,or(C1,C2)) ], Lu, Lu1), !.
evolue((I,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 			concat([ (I,C) ], Ls, Ls1), !.
evolue((I,not(C)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :- 		concat([ (I,not(C)) ], Ls, Ls1), !.




%% A FAIRE

resolution(Lie, Lpt, Li, Lu, Ls, Abr) :- 	.


deduction_all(Lie, [ (I1,all(R,C)) | Lpt], Li, Lu, Ls, Abr) :- 	
			member((I1,I2,R), Abr),
			
			evolue((I2,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
			
			deduction_all(Lie1, Lpt, Li1, Lu1, Ls1, Abr), !.



transformation_or(Lie,Lpt,Li,Lu,Ls,Abr) :- 	.


affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2, Lpt2, Li2, Lu2, Abr2) :- .
