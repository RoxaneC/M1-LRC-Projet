
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

%%%


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

