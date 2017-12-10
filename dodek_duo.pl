:-use_module(library(clpfd)), use_module(library(lists)).

all_diff(L) :- \+ (select(X,L,R), memberchk(X,R)).

is_set(Lst) :-
    setof(X, member(X, Lst), Set),
    length(Lst, N),
    length(Set, N).
	
	
comp(Face1,Face2,Counter):- Counter=3.

comp(Face1,Face2,Counter):-
	Counter<3,
	element(Counter,Face1,Elem1),
	element(Counter,Face2,Elem2),
	Elem1#=Elem2,
	NewCounter is Counter+1,
	comp(Face1,Face2,NewCounter).	
	
dodek(L):-

Face1=[A1,A2],
Face2=[B1,B2],
Face3=[C1,C2],

domain(Face1,1,2),
domain(Face2,1,2),
domain(Face3,1,2),

/*
all_different(Face1),
all_different(Face2),
all_different(Face3),
*/

all_different(Face1),
all_different(Face2),

comp(Face1,Face2,1),

/*
Faces = [A1,A2,A3,A4,A5,
		B1,B2,B3,B4,B5,
		C1,C2,C3,C4,C5,
		D1,D2,D3,D4,D5,
		E1,E2,E3,E4,E5,
		F1,F2,F3,F4,F5,
		G1,G2,G3,G4,G5,
		H1,H2,H3,H4,H5,
		I1,I2,I3,I4,I5,
		J1,J2,J3,J4,J5,
		L1,L2,L3,L4,L5,
		M1,M2,M3,M4,M5],
*/


append([Face1,Face2],L),
		
labeling([], L).