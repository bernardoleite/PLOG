
/*These Functions are responsible to check if it is possible to place a Black Piece 
considering all possible cases of Crosscut*/


fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 1,
							 NY is Y-1,
							 getPiece(I,X,NY,Elem1), Elem1 = 2,
							 NX is X-1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 2,
							 NX2 is X-1, NY2 is Y-1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 1,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							FINALval is VALUE1-VALUE2,
							FINALval=1.
							
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 0,
							 NY is Y-1,
							 getPiece(I,X,NY,Elem1), Elem1 = 2,
							 NX is X-1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 2,
							 NX2 is X-1, NY2 is Y-1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 1,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							FINALval is VALUE1-VALUE2,
							FINALval=1.
							 

fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 1,
							 NY is Y+1,
							 getPiece(I,X,NY,Elem1), Elem1 = 2,
							 NX is X-1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 2,
							 NX2 is X-1, NY2 is Y+1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 1, 
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.
							 
							 
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 0,
							 NY is Y+1,
							 getPiece(I,X,NY,Elem1), Elem1 = 2,
							 NX is X-1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 2,
							 NX2 is X-1, NY2 is Y+1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 1, 
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.

							 
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 1,
							 NY is Y-1,
							 getPiece(I,X,NY,Elem1), Elem1 = 2,
							 NX is X+1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 2,
							 NX2 is X+1, NY2 is Y-1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 1,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.

fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 0,
							 NY is Y-1,
							 getPiece(I,X,NY,Elem1), Elem1 = 2,
							 NX is X+1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 2,
							 NX2 is X+1, NY2 is Y-1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 1,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.
 
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 1,
							 NY is Y+1,
							 getPiece(I,X,NY,Elem1), Elem1 = 2,
							 NX is X+1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 2,
							 NX2 is X+1, NY2 is Y+1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 1,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.
							 
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 0,
							 NY is Y+1,
							 getPiece(I,X,NY,Elem1), Elem1 = 2,
							 NX is X+1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 2,
							 NX2 is X+1, NY2 is Y+1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 1,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.
							 



/*These Functions are responsible to check if it is possible to place a White Piece 
considering all possible cases of Crosscut*/





fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 2,
							 NY is Y-1,
							 getPiece(I,X,NY,Elem1), Elem1 = 1,
							 NX is X-1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 1,
							 NX2 is X-1, NY2 is Y-1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 2,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							FINALval is VALUE1-VALUE2,
							FINALval=1.
							
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 0,
							 NY is Y-1,
							 getPiece(I,X,NY,Elem1), Elem1 = 1,
							 NX is X-1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 1,
							 NX2 is X-1, NY2 is Y-1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 2,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							FINALval is VALUE1-VALUE2,
							FINALval=1.
							 

fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 2,
							 NY is Y+1,
							 getPiece(I,X,NY,Elem1), Elem1 = 1,
							 NX is X-1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 1,
							 NX2 is X-1, NY2 is Y+1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 2, 
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.
							 
							 
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 0,
							 NY is Y+1,
							 getPiece(I,X,NY,Elem1), Elem1 = 1,
							 NX is X-1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 1,
							 NX2 is X-1, NY2 is Y+1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 2, 
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.

							 
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 2,
							 NY is Y-1,
							 getPiece(I,X,NY,Elem1), Elem1 = 1,
							 NX is X+1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 1,
							 NX2 is X+1, NY2 is Y-1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 2,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.

fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 0,
							 NY is Y-1,
							 getPiece(I,X,NY,Elem1), Elem1 = 1,
							 NX is X+1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 1,
							 NX2 is X+1, NY2 is Y-1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 2,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.
 
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 2,
							 NY is Y+1,
							 getPiece(I,X,NY,Elem1), Elem1 = 1,
							 NX is X+1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 1,
							 NX2 is X+1, NY2 is Y+1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 2,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.
							 
fullCrossCut(C,I,X,Y,Jogador):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
							 getPiece(I,X,Y,Elem), Elem = 0,
							 NY is Y+1,
							 getPiece(I,X,NY,Elem1), Elem1 = 1,
							 NX is X+1,
							 getPiece(I,NX,Y,Elem2), Elem2 = 1,
							 NX2 is X+1, NY2 is Y+1,
							 getPiece(I,NX2,NY2,Elem3), Elem3 = 2,
							 getPiece(C,X,NY,VAL1), getPiece(C,NX,Y,VAL2),
							 getPiece(C,X,Y,VAL3), getPiece(C,NX2,NY2,VAL4),
							 VALUE1 is VAL1+VAL2, VALUE2 is VAL3+VAL4,
							 FINALval is VALUE1-VALUE2,
							 FINALval=1.							 
							 

							 

							 
							 

/*Get Stuff*/
getPeca(Tabuleiro, Nlinha, NCol, Peca) :-
	getElePos(NLinha, Tabuleiro, Linha),
	getElePos(NCol, Linha, Peca).

getElePos(1, [Elemento|_], Elemento).

getElePos(Pos, [_|Resto], Elemento) :-
	Pos > 1,
	Next is Pos-1,
	getElePos(Next, Resto, Elemento).
	
getPiece(Tab, Row, Column, Elem) :-    
   nth0(Row,Tab,ResultList),
   nth0(Column,ResultList,Elem).
   
/*Set Stuff*/
setPeca(TabIn, NLinha, NCol, Peca, TabOut) :-
	setNaLinha(NLinha, TabIn, NCol, Peca, TabOut).

setNaLinha(1, [Linha|Resto], NCol, Peca, [NovaLinha|Resto]):-
	setNaColuna(NCol, Linha, Peca, NovaLinha).

setNaLinha(Pos, [Linha|Resto], NCol, Peca, [Linha|NovoResto]):-
	Pos > 1,
	Next is Pos-1,
	setNaLinha(Next, Resto, NCol, Peca, NovaLinha).

setNaColuna(1, [_|Resto], Peca, [Peca|Resto]).

setNaColuna(Pos, [X|Resto], Peca, [X|NovoResto]):-
	Pos > 1,
	Next is Pos-1,
	setNaColuna(Next, Resto, Peca, NovoResto).
	
setPiece( L , X , Y , Z , R ) :-
  append(RowPfx,[Row|RowSfx],L),   
  length(RowPfx,X) ,                 
  append(ColPfx,[_|ColSfx],Row) ,   
  length(ColPfx,Y) ,                 
  append(ColPfx,[Z|ColSfx],RowNew) ,
  append(RowPfx,[RowNew|RowSfx],R).

copy(L,R) :- accCp(L,R).
accCp([],[]).
accCp([H|T1],[H|T2]) :- accCp(T1,T2).

setCounting(CountingBoard,Line,Column, NewBoard) :-  	
	getPiece(CountingBoard,Line,Column,Elem),
	if_then_else(Elem=0,setPiece(CountingBoard , Line,Column , 1 , NewBoard), write('')),
	if_then_else(Elem=1,setPiece(CountingBoard , Line,Column , 2 , NewBoard), write('')).
	
setIdentity(Jogador,IdentityBoard,Line,Column, NewBoard) :-	
	if_then_else(Jogador=1,setPiece(IdentityBoard , Line,Column , 1 , NewBoard), write('')),
	if_then_else(Jogador=2,setPiece(IdentityBoard , Line,Column , 2 , NewBoard), write('')).
						
 
  
 /*Compare*/
 
 isEqual(Elem1,Elem2) :- Elem1 == Elem2.
 
if_then_else(Condition, Action1, Action2) :- Condition, !, Action1.  
if_then_else(Condition, Action1, Action2) :- Action2.

and(A,B):- is_true(A), is_true(B).
or(A,B):- is_true(A) ; is_true(B).

is_true(true).     
is_true(A):- var(A), !, false.  
is_true(and(A,B)):- and(A,B).
is_true(or(A,B)):- or(A,B).    


replay(B,C,I,Jogador,Counter):- write('You cannot put that piece there!'), nl, stroke(B,C,I, Jogador, Counter).

/*-------------*/
initialBoard([	['0','0','0','0','0','0','0','0','0','0'],
				['0','0','0','0','0','0','0','0','0','0'],
				['0','0','0','0','0','0','0','0','0','0'],
				['0','0','0','0','0','0','0','0','0','0'],
				['0','0','0','0','0','0','0','0','0','0'],
				['0','0','0','0','0','0','0','0','0','0'],
				['0','0','0','0','0','0','0','0','0','0'],
				['0','0','0','0','0','0','0','0','0','0'],
				['0','0','0','0','0','0','0','0','0','0'],
				['0','0','0','0','0','0','0','0','0','0']
			]
				).
				
countingBoard([	[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0]
			]
				).

identityBoard([	[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0]
			]
				).					

startGame:- use_module(library(lists)), initialBoard(B), countingBoard(C), identityBoard(I), showBoard(B), stroke(B,C,I,1, 1).



stroke(B,C,I,Jogador, Counter):-Counter < 15, write('Player '), write(Jogador), write(' it is your turn!'), nl,
								write('Line: '), nl, read(X), nl, 
								write('Column: '), nl, read(Y), nl, 
								write('Piece: '), nl, read(P), nl, 
								write('Nr of Stroke: '), write(Counter), nl, NewCounter is Counter+1, 
								putPiece(B,C,I, X, Y, P, Jogador, NewCounter).

putPiece(B,C,I, X, Y, P, Jogador, Counter):- Jogador == 1,
						getPiece(B,X,Y,Elem),
						if_then_else(P\='b', replay(B,C,I,Jogador, Counter), write('')),
						if_then_else(Elem='B', replay(B,C,I,Jogador,Counter), write('')),
						if_then_else(Elem='W', replay(B,C,I,Jogador,Counter), write('')),
						if_then_else(Elem='w', replay(B,C,I,Jogador,Counter), write('')),		
						if_then_else(fullCrossCut(C,I,X,Y,Jogador), replay(B,C,I,Jogador,Counter), write('')),
						if_then_else(Elem='b', setPiece(B, X, Y, 'B', R), setPiece(B, X, Y, P, R)),
						setCounting(C,X,Y,NewCountingBoard),
						setIdentity(Jogador,I,X,Y, NewIdentityBoard),
						showBoard(R), nl,
						stroke(R,NewCountingBoard,NewIdentityBoard, 2, Counter). 
										
putPiece(B,C,I,X, Y, P, Jogador, Counter):- Jogador == 2,
										getPiece(B,X,Y,Elem),
										if_then_else(P\='w', replay(B,C,I,Jogador, Counter), write('')),
										if_then_else(Elem='W', replay(B,C,I,Jogador,Counter), write('')),
										if_then_else(Elem='B', replay(B,C,I,Jogador,Counter), write('')),
										if_then_else(Elem='b', replay(B,C,I,Jogador,Counter), write('')),
										if_then_else(fullCrossCut(C,I,X,Y,Jogador), replay(B,C,I,Jogador,Counter), write('')),
										if_then_else(Elem='w', setPiece(B, X, Y, 'W', R), setPiece(B, X, Y, P, R)),
										setCounting(C,X,Y,NewCountingBoard),
										setIdentity(Jogador,I,X,Y, NewIdentityBoard),
										showBoard(R), nl,
										stroke(R,NewCountingBoard,NewIdentityBoard, 1, Counter).



showBoard(Board) :-
	nl,
	write('***Current Board***'), nl,
	showLine,
	showRowByRow(Board),
	showLine.

showLine :-
	nl.

showRowByRow([]).
showRowByRow([Line|Rest]) :-
	showSingleRow(Line),
	showRowByRow(Rest).

showSingleRow([Cell]):-
	write(Cell),
	write('|'),
	showLine.

showSingleRow([Cell|More]):-
	write(Cell),
	write('|'),
	showSingleRow(More).