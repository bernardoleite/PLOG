:-use_module(library(clpfd)), use_module(library(lists)), use_module(library(random)).


sublist(L1, L2, I, J):-
    sublist(L1, Temp, I, J, []),
    !,
    reverse(Temp, L2).
sublist([], L2, _I, _J, L2).
sublist(_L1, L2, I, J, L2):-
    I > J.
sublist(L1, L2, I, J, L2):-
    I < 0,
    sublist(L1, L2, 0, J, L2).
sublist([_L|Ls], L2, I, J, Acc):-
    I > 0,
    sublist(Ls, L2, I-1, J-1, Acc).
sublist([L|Ls], L2, I, J, Acc):-
    sublist(Ls, L2, I, J-1, [L|Acc]).
	
removeAll(_, [], []).
removeAll(X, [X|T], L):- removeAll(X, T, L), !.
removeAll(X, [H|T], [H|L]):- removeAll(X, T, L ).

/*Generates all Combinations Given a List and N*/
combination(0, _, []) :- 
    !.
combination(N, L, [V|R]) :-
    N > 0,
    NN is N - 1,
    unknown(V, L, Rem),
    combination(NN, Rem, R).

unknown(X,[X|L],L).
unknown(X,[_|L],R) :- 
    unknown(X,L,R).
	

allListsDifferent2([],[],[]).
allListsDifferent2([H1|T1],[H2|T2],[HB|TB]):-
    H1 #= H2 #<=> HB,
    allListsDifferent2(T1,T2,TB).

allListsDifferent1(_,[]).
allListsDifferent1(HA,[HB|T]):-
    length(HA,N),
    length(LB,N),
    domain(LB,0,1),
    sum(LB,#<,N),
    allListsDifferent2(HA,HB,LB),
    allListsDifferent1(HA,T).

allListsDifferent([_]).
allListsDifferent([HA,HB|T]):-
    allListsDifferent1(HA,[HB|T]),
    allListsDifferent([HB|T]).

shift(L1, N, L2) :-
    N < 0, !,
    N1 is -N,
    shift(L2, N1, L1).  

shift(L1, N, L2) :- 
    append(Lx, Ly, L1), % L1 is Lx || Ly
    append(Ly, Lx, L2), % L2 is Ly || Lx
    length(Lx, N).      % The length of Lx is N

generateAllPentagons(Pentagons,Tam,AuxList,Result,Counter):- Counter=Tam, Result=AuxList.

generateAllPentagons(Pentagons,Tam,AuxList,Result,Counter):-
	Counter<Tam,
	nth0(Counter,Pentagons,Penta),
	shift(Penta,0,Res),shift(Penta,1,Res1),shift(Penta,2,Res2),shift(Penta,3,Res3),shift(Penta,4,Res4),
	append([[Res,Res1,Res2,Res3,Res4],AuxList],NewAuxList), 
	NewCounter is Counter+1,
generateAllPentagons(Pentagons,Tam,NewAuxList,Result,NewCounter).


naive_sort(List,Sorted):-perm(List,Sorted),is_sorted(Sorted).

is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|T]):-X=<Y,is_sorted([Y|T]).


perm(List,[H|Perm]):-delete(H,List,Rest),perm(Rest,Perm).
perm([],[]).

delete(X,[X|T],T).
delete(X,[H|T],[H|NT]):-delete(X,T,NT).

if_then_else(Condition, Action1, Action2) :- Condition, !, Action1.  
if_then_else(Condition, Action1, Action2) :- Action2.

continueplay:- write('').

getOriginalPentagons(OriginalPentagonsColor):-
Penta1=[1,2,5,4,3],Penta2=[1,5,3,2,4],Penta3=[4,5,1,3,2],
Penta4=[4,3,5,2,1],Penta5=[2,3,4,1,5],Penta6=[1,2,3,4,5],
Penta7=[3,5,4,2,1],Penta8=[4,1,3,2,5],Penta9=[3,2,1,5,4],
Penta10=[1,4,2,5,3],Penta11=[2,5,1,4,3],Penta12=[4,5,3,2,1],
OriginalPentagonsColor=[Penta1,Penta2,Penta3,Penta4,Penta5,Penta6,Penta7,Penta8,Penta9,Penta10,Penta11,Penta12].



buildGenericPentagon(NrColors,AuxList,GenericPentagon,Counter):- Counter>NrColors, GenericPentagon=AuxList.
buildGenericPentagon(NrColors,AuxList,GenericPentagon,Counter):-
	Counter=<NrColors,
	append(AuxList,[Counter],NewAuxList),
	NewCounter is Counter+1,
buildGenericPentagon(NrColors,NewAuxList,GenericPentagon,NewCounter).

comparePairs(Pairs,Tam,Counter,AuxList,ConcreteList):- Counter=Tam,ConcreteList=AuxList.
comparePairs(Pairs,Tam,Counter,AuxList,ConcreteList):-
	Counter < Tam,
	nth0(Counter,Pairs,Pair),
	nth0(0,Pair,Pent1),nth0(1,Pair,Pent2),
	if_then_else(
	(rotate_list(1,Pent1,Pent2);rotate_list(2,Pent1,Pent2);
	rotate_list(3,Pent1,Pent2);rotate_list(4,Pent1,Pent2)),
	if_then_else(member(Pent2,AuxList), removeAll(Pent2,AuxList,NewAuxList), NewAuxList=AuxList),
	NewAuxList=AuxList),
	NewCounter is Counter+1,
comparePairs(Pairs,Tam,NewCounter,NewAuxList,ConcreteList).
	



removeRots(AllPermutations,ConcreteList):-
	findall(Pair,combination(2,AllPermutations,Pair),Pairs),
	length(Pairs,Tam),AuxList=AllPermutations,
	comparePairs(Pairs,Tam,0,AuxList,ConcreteList).
	
	


buildPentagons(NrPentagons,NrColors,BuiltPentagons):-
	buildGenericPentagon(NrColors,[],GenericPentagon,1),
	findall(Permutation, perm(GenericPentagon,Permutation), AllPermutations),
	removeRots(AllPermutations,ConcreteList),
	length(ConcreteList,Tam),
	MaxRandom is Tam-NrPentagons,
	random(0, MaxRandom, BeginList),
	EndList is BeginList+NrPentagons-1,
	sublist(ConcreteList, BuiltPentagons, BeginList, EndList).


menu:-  getOriginalPentagons(OriginalPentagonsColor),
		nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
		write('***Welcome to Dodek Duo Puzzle!***') ,nl,nl,
		write('There are two Puzzles:') ,nl,
		write('Puzzle 1: The aim is to assemble the puzzle so that at every edge two triangles of the same colour meet.') ,nl,
		write('Puzzle 2: The aim is to assemble the puzzle so that at every edge identical shapes meet.') ,nl,
		write('What do you want to do?'), nl,nl,
		write('Options:'),nl,
		write('(1) -> Solve Puzzle 1 (Same Color Meet)'),nl,
		write('(2) -> Solve Puzzle 2 (Same Shapes Meet)'),nl,
		write('Your Option (Select 1 or 2): '), nl,read(Puzzle),nl,nl,
		if_then_else(Puzzle=1, write('How do you want to solve this Puzzle?:'), continueplay) ,nl,
		if_then_else(Puzzle=1, write('(1) -> With the 12 Pentagonal Faces and 5 Colors from Original Puzzle.'), continueplay) ,nl,nl,
		if_then_else(Puzzle=1, write('(2) -> I want to choose how many Pentagons and how many Colors'), continueplay) ,nl,nl,
		write('Your Option (Select 1 or 2): '), nl, read(TypePentagons),nl,nl,
		if_then_else((Puzzle=1,TypePentagons=1), dodek_duo(Puzzle,OriginalPentagonsColor,Solution), continueplay) ,nl, 
		if_then_else((Puzzle=1,TypePentagons=2), (write('How many Pentagons? (Min is 12 and Max is 120) :  '), read(NrPentagons)), continueplay) ,nl,nl,
		if_then_else((Puzzle=1,TypePentagons=2), (write('How many Colors?: (Minimum is 5) : '), read(NrColors)), continueplay) ,nl,nl,
		if_then_else((Puzzle=1,TypePentagons=2), (buildPentagons(NrPentagons,NrColors,BuiltPentagons),dodek_duo(Puzzle,BuiltPentagons,Solution)), continueplay) ,nl,nl.

	
dodek_duo(Puzzle,Pentagons,Solution):-

length(Pentagons,Tam), 
generateAllPentagons(Pentagons,Tam,[],AllPenta,0),


%1-Laranja 2-Azul 3-Verde 4-Amarelo, 5-Rosa

%1-Retangulo 2-Semi-Circulo 3-Triangulo

%A definir pelo utilizador.......


/*
Penta1=[1,2,3,3,1],
Penta2=[1,2,1,3,3],
Penta3=[3,1,2,2,3],
Penta4=[3,2,2,1,1],
Penta5=[3,3,2,2,1],
Penta6=[3,1,1,2,2],
Penta7=[2,3,2,1,1],
Penta8=[2,3,3,2,1],
Penta9=[3,3,2,1,1],
Penta10=[3,1,1,3,2],
Penta11=[2,3,2,1,3],
Penta12=[1,2,1,2,3],
*/


%Creating All Faces. According to the figure this is read from top to bottom and left to right

Face1=[A1,A2,A3,A4,A5],
Face2=[B1,B2,B3,B4,B5],
Face3=[C1,C2,C3,C4,C5],
Face4=[D1,D2,D3,D4,D5],
Face5=[E1,E2,E3,E4,E5],
Face6=[F1,F2,F3,F4,F5],
Face7=[G1,G2,G3,G4,G5],
Face8=[H1,H2,H3,H4,H5],
Face9=[I1,I2,I3,I4,I5],
Face10=[J1,J2,J3,J4,J5],
Face11=[L1,L2,L3,L4,L5],
Face12=[M1,M2,M3,M4,M5],

table([Face1],AllPenta),
table([Face2],AllPenta),
table([Face3],AllPenta),
table([Face4],AllPenta),
table([Face5],AllPenta),
table([Face6],AllPenta),
table([Face7],AllPenta),
table([Face8],AllPenta),
table([Face9],AllPenta),
table([Face10],AllPenta),
table([Face11],AllPenta),
table([Face12],AllPenta),

%Two faces that are adjacent must have the same color

A1#=B1 #/\ A2#=F1 #/\ A3#=E1 #/\ A4#=D1 #/\ A5#=C1 #/\

B1#=A1 #/\ B2#=C5 #/\ B3#=H5 #/\ B4#=G1 #/\ B5#=F2 #/\

C1#=A5 #/\ C2#=D5 #/\ C3#=I5 #/\ C4#=H1 #/\ C5#=B2 #/\

D1#=A4 #/\ D2#=E5 #/\ D3#=J5 #/\ D4#=I1 #/\ D5#=C2 #/\

E1#=A3 #/\ E2#=F5 #/\ E3#=L5 #/\ E4#=J1 #/\ E5#=D2 #/\

F1#=A2 #/\ F2#=B5 #/\ F3#=G5 #/\ F4#=L1 #/\ F5#=E2 #/\

G1#=B4 #/\ G2#=H4 #/\ G3#=M5 #/\ G4#=L2 #/\ G5#=F3 #/\

H1#=C4 #/\ H2#=I4 #/\ H3#=M1 #/\ H4#=G2 #/\ H5#=B3 #/\

I1#=D4 #/\ I2#=J4 #/\ I3#=M2 #/\ I4#=H2 #/\ I5#=C3 #/\

J1#=E4 #/\ J2#=L4 #/\ J3#=M3 #/\ J4#=I2 #/\ J5#=D3 #/\

L1#=F4 #/\ L2#=G4 #/\ L3#=M4 #/\ L4#=J2 #/\ L5#=E3 #/\

M1#=H3 #/\ M2#=I3 #/\ M3#=J3 #/\ M4#=L3 #/\ M5#=G3,

%Applying domain from 1 to 5 refering to the five colors that compose a face

domain(Face1,1,5),
domain(Face2,1,5),
domain(Face3,1,5),
domain(Face4,1,5),
domain(Face5,1,5),
domain(Face6,1,5),
domain(Face7,1,5),
domain(Face8,1,5),
domain(Face9,1,5),
domain(Face10,1,5),
domain(Face11,1,5),
domain(Face12,1,5),


%Check if all the triangles from a face have different colors

all_different(Face1),
all_different(Face2),
all_different(Face3),
all_different(Face4),
all_different(Face5),
all_different(Face6),
all_different(Face7),
all_different(Face8),
all_different(Face9),
all_different(Face10),
all_different(Face11),
all_different(Face12),



%Check if faces are different, including Rotations

AllFaces=[Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12],

generateAllPentagons(AllFaces,12,[],AllRots,0),

allListsDifferent(AllRots),

append([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12],Solution),
findall(Solution,labeling([], Solution),AllSol).