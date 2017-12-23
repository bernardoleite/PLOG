:-use_module(library(clpfd)), use_module(library(lists)), use_module(library(random)).
:-include(dodekduo_Puzzle1_Sol).
:-include(dodekduo_Puzzle2_Sol).
:-include(dodekduo_Puzzle1_Gen).
:-include(dodekduo_Puzzle2_Gen).


playProblem1(Solution,Pentagons,5,0):-
	if_then_else(dodekduo_Puzzle1_Sol(Solution,Pentagons,5,0),continueplay,noMoreSol).

	
playProblem2(Solution,Pentagons,3,0):-
	if_then_else(dodekduo_Puzzle2_Sol(Solution,Pentagons,3,0),continueplay,noMoreSol).


forceColors(AllElements,MaxColor,Counter):-Counter=MaxColor.
forceColors(AllElements,MaxColor,Counter):-
	Counter<MaxColor,
	element(X,AllElements,Counter),
	NewCounter is Counter+1,
forceColors(AllElements,MaxColor,NewCounter).

noMoreSol:- nl,nl,write('!!!No More Solutions!!!'),nl,nl,
			menu.

checkAppears1PerFace(AllElements,Figure):-

	nth1(1,AllElements,Face1), count(Figure,Face1,#=,Count1), Count1#>=1,Count1#=<2,
	nth1(2,AllElements,Face2), count(Figure,Face2,#=,Count2), Count2#>=1,Count2#=<2,
	nth1(3,AllElements,Face3), count(Figure,Face3,#=,Count3), Count3#>=1,Count3#=<2,
	nth1(4,AllElements,Face4), count(Figure,Face4,#=,Count4), Count4#>=1,Count4#=<2,
	nth1(5,AllElements,Face5), count(Figure,Face5,#=,Count5), Count5#>=1,Count5#=<2,
	nth1(6,AllElements,Face6), count(Figure,Face6,#=,Count6), Count6#>=1,Count6#=<2,
	nth1(7,AllElements,Face7), count(Figure,Face7,#=,Count7), Count7#>=1,Count7#=<2,
	nth1(8,AllElements,Face8), count(Figure,Face8,#=,Count8), Count8#>=1,Count8#=<2,
	nth1(9,AllElements,Face9), count(Figure,Face9,#=,Count9), Count9#>=1,Count9#=<2,
	nth1(10,AllElements,Face10), count(Figure,Face10,#=,Count10), Count10#>=1,Count10#=<2,
	nth1(11,AllElements,Face11), count(Figure,Face11,#=,Count11), Count11#>=1,Count11#=<2,
	nth1(12,AllElements,Face12), count(Figure,Face12,#=,Count12), Count12#>=1,Count12#=<2.

forceShapes(AllElements,MaxFigures,Counter):-Counter=MaxFigures.
forceShapes(AllElements,MaxFigures,Counter):-
	Counter<MaxFigures,
	checkAppears1PerFace(AllElements,Counter),
	NewCounter is Counter+1,
forceShapes(AllElements,MaxFigures,NewCounter).

same([], []).

same([H1|R1], [H2|R2]):-
    H1 = H2,
    same(R1, R2).
	
iteratePenta(Solution,Aux,Size,Counter,Problem):- Counter=Size, random_permutation(Aux,Problem).
iteratePenta(Solution,Aux,Size,Counter,Problem):-
	Counter<Size,
	nth0(Counter,Solution,Penta),
	random(0,4,Rot),
	shift(Penta,Rot,NewPenta),
	append(Aux,[NewPenta],NewAux),
	NewCounter is Counter+1,
iteratePenta(Solution,NewAux,Size,NewCounter,Problem).

iterateProblem(Problem,[],Size,Counter):- Counter=Size.
iterateProblem(Problem,[],Size,Counter):-
	Counter<Size,
	nth1(Counter,Problem,Penta),
	write('Penta '), write(Counter), write(' = '), write(Penta), nl,
	NewCounter is Counter + 1,
iterateProblem(Problem,[],Size,NewCounter).

printProblem(Problem):-
	length(Problem,S),
	Size is S+1,
	iterateProblem(Problem,[],Size,1).
	
	

discoverPenta(Face,Problem,Counter,Penta):-
	nth1(Counter,Problem,CurrPent),
	shift(CurrPent,0,Res0),shift(CurrPent,1,Res1),shift(CurrPent,2,Res2),shift(CurrPent,3,Res3),shift(CurrPent,4,Res4),
	if_then_else((same(Face,Res0);same(Face,Res1);same(Face,Res2);same(Face,Res3);same(Face,Res4)),
				 Penta is Counter,
				(NewCounter is Counter+1, discoverPenta(Face,Problem,NewCounter,Penta))).
	
	
	
	
iterateSolution(Solution,Problem,Size,Counter,Numerator):- Counter=Size.
iterateSolution(Solution,Problem,Size,Counter,Numerator):-
	Counter<Size, 
	Counter2 is Counter+1, Counter3 is Counter+2, Counter4 is Counter+3, Counter5 is Counter + 4,
	nth1(Counter,Solution,Elem1),nth1(Counter2,Solution,Elem2),nth1(Counter3,Solution,Elem3),nth1(Counter4,Solution,Elem4),nth1(Counter5,Solution,Elem5),
	Face=[Elem1,Elem2,Elem3,Elem4,Elem5],
	discoverPenta(Face,Problem,1,Penta),
	write('Face '), write(Numerator), write(' = '), write(Face), write(' ... Penta Used : ('), write(Penta), write(') '), nl,
	NewCounter is Counter + 5,
	NewNumerator is Numerator+1,
iterateSolution(Solution,Problem,Size,NewCounter,NewNumerator).	
	
printSolution(Problem,Solution):-
	length(Solution,S),
	Size is S+1,
	iterateSolution(Solution,Problem,Size,1,1).
	
	
	
	
generateProb(Solution,Problem):-
	length(Solution,Size),
	iteratePenta(Solution,[],Size,0,Problem).


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

getProblem1Penta(Pentagons):- 
	Penta1=[1,2,5,4,3],Penta2=[1,5,3,2,4],Penta3=[4,5,1,3,2],
	Penta4=[4,3,5,2,1],Penta5=[2,3,4,1,5],Penta6=[1,2,3,4,5],
	Penta7=[3,5,4,2,1],Penta8=[4,1,3,2,5],Penta9=[3,2,1,5,4],
	Penta10=[1,4,2,5,3],Penta11=[2,5,1,4,3],Penta12=[4,5,3,2,1],
	Pentagons=[Penta1,Penta2,Penta3,Penta4,Penta5,Penta6,Penta7,Penta8,Penta9,Penta10,Penta11,Penta12].

getProblem2Penta(Pentagons):- 
	Penta1=[1,2,3,3,1],Penta2=[1,2,1,3,3],Penta3=[3,1,2,2,3],Penta4=[3,2,2,1,1],
	Penta5=[3,3,2,2,1],Penta6=[3,1,1,2,2],Penta7=[2,3,2,1,1],Penta8=[2,3,3,2,1],
	Penta9=[3,3,2,1,1],Penta10=[3,1,1,3,2],Penta11=[2,3,2,1,3],Penta12=[1,2,1,2,3],
	Pentagons=[Penta1,Penta2,Penta3,Penta4,Penta5,Penta6,Penta7,Penta8,Penta9,Penta10,Penta11,Penta12].
	
	
if_then_else(Condition, Action1, Action2) :- Condition, !, Action1.  
if_then_else(Condition, Action1, Action2) :- Action2.

continueplay:- write('').
error:- write('!!!Please, check your input!!!'),menu.

menu:-  getProblem1Penta(Pentagons1),getProblem2Penta(Pentagons2),
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
		if_then_else((Puzzle=1;Puzzle=2), write('How do you want to do with this Puzzle?:'), continueplay) ,nl,
		if_then_else(Puzzle=1, write('(1) -> See Solution(s) of the Original Game.'), continueplay) ,nl,nl,
		if_then_else(Puzzle=1, write('(2) -> Randomly generate a problem to be solved.'), continueplay) ,nl,nl,
		if_then_else(Puzzle=2, write('(1) -> See Solution(s) of the Original Game.'), continueplay) ,nl,nl,
		if_then_else(Puzzle=2, write('(2) -> Randomly generate a problem to be solved.'), continueplay) ,nl,nl,		
		write('Your Option (Select 1 or 2): '), nl, read(Puzzle_Option),nl,nl,
		if_then_else((Puzzle=1,Puzzle_Option=1), playProblem1(Solution,Pentagons1,5,0), continueplay) ,nl, 
		if_then_else((Puzzle=1,Puzzle_Option=2), (write('How many Colors?: (Minimum is 5 and Maximum is 12)  '), nl,read(NrColors)), continueplay) ,nl,nl,
		if_then_else((Puzzle=2,Puzzle_Option=2), (write('How many Figures?: (Minimum is 3 and Maximum is 5)  '), nl,read(NrFigures)), continueplay) ,nl,nl,
		if_then_else((Puzzle=1,Puzzle_Option=2,(NrColors<5; NrColors>12)), error, continueplay) ,nl,nl,
		if_then_else((Puzzle=2,Puzzle_Option=2,(NrFigures<3; NrFigures>5)), error, continueplay) ,nl,nl,

		if_then_else((Puzzle=1,Puzzle_Option=2), dodekduo_Puzzle1_Gen(Solution,1,NrColors), continueplay) ,nl,nl,
		if_then_else((Puzzle=2,Puzzle_Option=2), dodekduo_Puzzle2_Gen(Solution,1,NrFigures), continueplay),
		
		if_then_else((Puzzle=2,Puzzle_Option=1), playProblem2(Solution,Pentagons2,3,0), continueplay).
		