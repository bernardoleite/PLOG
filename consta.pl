
/*These Functions are responsible to check if it is possible to place a Black Piece 
considering all possible cases of Crosscut*/


fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
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
							FINALval=1, 
							 Bool is 0.
							
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
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
							FINALval=1,
							Bool is 1.
							 

fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							  Bool is 0.
							 
							 
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							 Bool is 1.

							 
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							  Bool is 0.

fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							 Bool is 1.
 
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
								 Bool is 0.
							 
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 1, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							 Bool is 1.
							 



/*These Functions are responsible to check if it is possible to place a White Piece 
considering all possible cases of Crosscut*/





fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
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
							FINALval=1,
								 Bool is 0.
							
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
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
							FINALval=1,
							Bool is 1.
							 

fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							  Bool is 0.
							 
							 
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							 Bool is 1.

							 
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							 Bool is 0.

fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							 Bool is 1.
 
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							  Bool is 0.
							 
fullCrossCut(C,I,X,Y,Jogador,Move,Bool):-  Jogador == 2, X>=0, X=<9, Y>=0, Y=<9,
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
							 FINALval=1,
							 Bool is 1.							 

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
						
 
 isEqual(Elem1,Elem2) :- Elem1 == Elem2.
 
if_then_else(Condition, Action1, Action2) :- Condition, !, Action1.  
if_then_else(Condition, Action1, Action2) :- Action2.

and(A,B):- is_true(A), is_true(B).
or(A,B):- is_true(A) ; is_true(B).

is_true(true).     
is_true(A):- var(A), !, false.  
is_true(and(A,B)):- and(A,B).
is_true(or(A,B)):- or(A,B).    


/*Check if there is connection between two pieces on Board (ortogonally or diagonaly)*/

existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2),
									Elem1=Jogador,
									Elem1=Elem2,
									Y1=Y2,
									LineDifference is abs(X1-X2),
									LineDifference=1.

existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2), 
									Elem1=Jogador,
									Elem1=Elem2,
									X1=X2,
									ColumnDifference is abs(Y1-Y2),
									ColumnDifference=1.

existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2),
									Elem1=Jogador,
									Elem1=Elem2,
									ColumnDifference is Y2-Y1,
									ColumnDifference=1,
									LineDifference is X1-X2,
									LineDifference=1,
									getPiece(C,X1,Y1,VAL1),getPiece(C,X2,Y2,VAL2),
									X3 is X1, Y3 is Y2,			
									getPiece(C,X3,Y3,VAL3),
									X4 is X2, Y4 is Y1,
									getPiece(C,X4,Y4,VAL4),
									SUM1 is VAL1+VAL2, SUM2 is VAL3+VAL4,
									SUM1>SUM2,
									getPiece(I,X3,Y3,Elem3),
									getPiece(I,X4,Y4,Elem4),
									Elem1\=Elem3, Elem1\=Elem4.
									
existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2),
									Elem1=Jogador,
									Elem1=Elem2,
									ColumnDifference is Y1-Y2,
									ColumnDifference=1,
									LineDifference is X1-X2,
									LineDifference=1,
									getPiece(C,X1,Y1,VAL1),getPiece(C,X2,Y2,VAL2),
									X3 is X1, Y3 is Y2,
									getPiece(C,X3,Y3,VAL3),
									X4 is X2, Y4 is Y1,
									getPiece(C,X4,Y4,VAL4),
									SUM1 is VAL1+VAL2, SUM2 is VAL3+VAL4,
									SUM1>SUM2,
									getPiece(I,X3,Y3,Elem3),
									getPiece(I,X4,Y4,Elem4),
									Elem1\=Elem3, Elem1\=Elem4.
									

existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2),
									Elem1=Jogador,
									Elem1=Elem2,
									ColumnDifference is Y1-Y2,
									ColumnDifference=1,
									LineDifference is X2-X1,
									LineDifference=1,
									getPiece(C,X1,Y1,VAL1),getPiece(C,X2,Y2,VAL2),
									X3 is X1, Y3 is Y2,
									getPiece(C,X3,Y3,VAL3),
									X4 is X2, Y4 is Y1,
									getPiece(C,X4,Y4,VAL4),
									SUM1 is VAL1+VAL2, SUM2 is VAL3+VAL4,
									SUM1>SUM2,
									getPiece(I,X3,Y3,Elem3),
									getPiece(I,X4,Y4,Elem4),
									Elem1\=Elem3, Elem1\=Elem4.
									
existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2),
									Elem1=Jogador,
									Elem1=Elem2,
									ColumnDifference is Y2-Y1,
									ColumnDifference=1,
									LineDifference is X2-X1,
									LineDifference=1,
									getPiece(C,X1,Y1,VAL1),getPiece(C,X2,Y2,VAL2),
									X3 is X1, Y3 is Y2,
									getPiece(C,X3,Y3,VAL3),
									X4 is X2, Y4 is Y1,
									getPiece(C,X4,Y4,VAL4),
									SUM1 is VAL1+VAL2, SUM2 is VAL3+VAL4,
									SUM1>SUM2,
									getPiece(I,X3,Y3,Elem3),
									getPiece(I,X4,Y4,Elem4),
									Elem1\=Elem3, Elem1\=Elem4.
									
								

	   
							
		
/*								
existsCycle(X,Y,B,C,I,P,Jogador):- X>0, Y>0, X<9, Y<9,
								   copy(B,B2), copy(C,C2), copy(I,I2),
								   getPiece(B,X,Y,Elem),
								   if_then_else(Elem='w', setPiece(B2, X, Y, 'W', R), setPiece(B2, X, Y, P, R)),
								   setCounting(C2,X,Y,NewCountingBoard),
								   setIdentity(Jogador,I2,X,Y, NewIdentityBoard),
								   empty(List), push([X,Y],List,NewList),
								   travelPiece(X,Y,NewCountingBoard,NewIdentityBoard,NewList).
									

travelPiece(X,Y,C,I,List):- 	
							PX1 is X, PY1 is Y+1,
							existsConnection(C,I,X,Y,PX1,PX2),
							non_member([PX1,PX2],List),
							push([PX1,PX2],List,NewList),
							travelPiece(PX1,PY1,C,I,NewList).
							
travelPiece(X,Y,C,I,List):- 	
							PX1 is X-1, PY1 is Y,
							existsConnection(C,I,X,Y,PX1,PX2),
							non_member([PX1,PX2],List),
							push([PX1,PX2],List,NewList),
							travelPiece(PX1,PY1,C,I,NewList).
							

travelPiece(X,Y,C,I,List):- 	
							PX1 is X, PY1 is Y-1,
							existsConnection(C,I,X,Y,PX1,PX2),
							non_member([PX1,PX2],List),
							push([PX1,PX2],List,NewList),
							travelPiece(PX1,PY1,C,I,NewList).
							
travelPiece(X,Y,C,I,List):- 	
							PX1 is X+1, PY1 is Y,
							existsConnection(C,I,X,Y,PX1,PX2),
							non_member([PX1,PX2],List),
							push([PX1,PX2],List,NewList),
							travelPiece(PX1,PY1,C,I,NewList).
*/									 
empty([]).
pop(E, [E|Es],Es).
push(E, Es, [E|Es]).

/* Graph Stuff */
connected(X,Y) :- edge(X,Y).
connected(X,Y) :- edge(Y,X).

path(A,B,Path) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path).

travel(A,B,P,[B|P]) :- 
       connected(A,B).
travel(A,B,Visited,Path) :-
       connected(A,C),           
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path). 
	   
	   
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
	
/*findall(L, combination(2,[a,b,c,d],L), R).*/

/*Auxiliar Lists*/

list_pairs(List1, List2, Pairs) :-
    findall([X,Y], (member(X, List1), member(Y, List2)), Pairs).

allCords([ 
			[0,0], [0,1], [0,2], [0,3], [0,4],[0,5],[0,6],[0,7],[0,8],[0,9],
			[1,0], [1,1], [1,2], [1,3], [1,4],[1,5],[1,6],[1,7],[1,8],[1,9],
			[2,0], [2,1], [2,2], [2,3], [2,4],[2,5],[2,6],[2,7],[2,8],[2,9],
			[3,0], [3,1], [3,2], [3,3], [3,4],[3,5],[3,6],[3,7],[3,8],[3,9],
			[4,0], [4,1], [4,2], [4,3], [4,4],[4,5],[4,6],[4,7],[4,8],[4,9],
			[5,0], [5,1], [5,2], [5,3], [5,4],[5,5],[5,6],[5,7],[5,8],[5,9],
			[6,0], [6,1], [6,2], [6,3], [6,4],[6,5],[6,6],[6,7],[6,8],[6,9],
			[7,0], [7,1], [7,2], [7,3], [7,4],[7,5],[7,6],[7,7],[7,8],[7,9],
			[8,0], [8,1], [8,2], [8,3], [8,4],[8,5],[8,6],[8,7],[8,8],[8,9],
			[9,0], [9,1], [9,2], [9,3], [9,4],[9,5],[9,6],[9,7],[9,8],[9,9]
			]).
			
blackCoordsTop([[0,0], [0,1], [0,2], [0,3], [0,4],[0,5],[0,6],[0,7],[0,8],[0,9]]).
blackCoordsBottom([[9,0], [9,1], [9,2], [9,3], [9,4],[9,5],[9,6],[9,7],[9,8],[9,9]]).

whiteCoordsLeft([[0,0], [1,0], [2,0], [3,0], [4,0],[5,0],[6,0],[7,0],[8,0],[9,0]]).
whiteCoordsRight([[0,1], [1,9], [2,9], [3,9], [4,9],[5,9],[6,9],[7,9],[8,9],[9,9]]).



/*-------------*/

searchVertiCally(ResultList,Counter):-
						Counter=<99,
						nth0(Counter,ResultList,Pair),
						nth0(0,Pair,Point1), nth0(1,Pair,Point2),
						if_then_else(path(Point1,Point2,Path), write(Path), write('')), nl,
						NewCounter is Counter+1,
						searchVertiCally(ResultList,NewCounter).


createsEdge(Point1,Point2,Flag):- assert(edge(Point1,Point2)), Flag is 1.

buildEdges(B,C,I,Jogador,Counter,ResultList,Flag):- 
						Counter=<4949,
						nth0(Counter,ResultList,Pair),
						nth0(0,Pair,Point1), nth0(1,Pair,Point2),
						nth0(0,Point1,P1x), nth0(1,Point1, P1y),
						nth0(0,Point2,P2x), nth0(1,Point2, P2y), 
						if_then_else(existsConnection(Jogador,C,I,P1x,P1y,P2x,P2y), createsEdge(Point1,Point2,Flag), write('')),
						NewCounter is Counter+1,
						buildEdges(B,C,I,Jogador,NewCounter,ResultList,Flag).
												
		
findPaths(Jogador):- Jogador==1,
			blackCoordsTop(List1), blackCoordsBottom(List2),
			list_pairs(List1,List2, Pairs),
			write(Pairs),nl,
			if_then_else(searchVertiCally(Pairs,0),write(''),write('')).
			
			

victory(B,C,I,Jogador):- Jogador==1,
						allCords(ListCoords),
						findall(L, combination(2,ListCoords,L), ResultList),
						if_then_else(buildEdges(B,C,I,Jogador,0,ResultList,Flag), write(''), write('')),
						if_then_else(findPaths(Jogador), write(''),write('')),
						if_then_else(Flag=1, retractall(edge(X,Y)), write('')).
						
							
							

			
initialBoard([	[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
				[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
				[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
				[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
				[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
				[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
				[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
				[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
				[' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
				[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
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

startGame:- use_module(library(lists)), initialBoard(B), countingBoard(C), identityBoard(I), showBoard(B), stroke(B,C,I,1,1,1,0,0,0).

replay(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY):- write('You cannot put that piece there!'), nl, stroke(B,C,I, Jogador, Counter,Move,Bool,LASTX,LASTY).

verify1(C,I,X,Y,Jogador,Move,NewBool):- fullCrossCut(C,I,X,Y,Jogador,Move,Boolean), 
									NewBool is Boolean,
									Move==1, 
									Boolean==1.
									
verify2(C,I,X,Y,Jogador,Move,NewBool):- fullCrossCut(C,I,X,Y,Jogador,Move,Boolean), 
									 Move==1, 
									 Boolean==0.
									 
verify2(C,I,X,Y,Jogador,Move,NewBool):- fullCrossCut(C,I,X,Y,Jogador,Move,Boolean), 
									 Move==2, 
									 Boolean==0.
									
									 
verify2(C,I,X,Y,Jogador,Move,NewBool):- fullCrossCut(C,I,X,Y,Jogador,Move,Boolean), 
									 Move==2, 
									 Boolean==1.
									 
									 
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Bool==1, Move==2, X\=LASTX, Y\=LASTY.


stroke(B,C,I,Jogador, Counter, Move, Bool, LASTX, LASTY):-
								Counter < 100, write('Player '), write(Jogador), write(' it is your turn!'), nl,
								write('Line: '), nl, read(X), nl, 
								write('Column: '), nl, read(Y), nl, 
								write('Piece: '), nl, read(P), nl, 
								write('Nr of Stroke: '), write(Counter), nl, NewCounter is Counter+1, 
								putPiece(B,C,I, X, Y, P, Jogador, NewCounter, Move,Bool,LASTX,LASTY).

putPiece(B,C,I, X, Y, P, Jogador, Counter, Move,Bool,LASTX,LASTY):- 
								Jogador == 1,
								getPiece(B,X,Y,Elem),
								if_then_else(P\='b', replay(B,C,I,Jogador, Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='B', replay(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='W', replay(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='w', replay(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),		
						
								if_then_else( verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY), replay(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(verify1(C,I,X,Y,Jogador,Move,NewBool), NEWLASTX is X, NEWLASTX is 0),
								if_then_else(verify1(C,I,X,Y,Jogador,Move,NewBool), NEWLASTY is Y, NEWLASTY is 0),
								if_then_else(verify2(C,I,X,Y,Jogador,Move,NewBool), replay(B,C,I,Jogador,Counter,Move,NewBool,NEWLASTX,NEWLASTY), write('')),


								if_then_else(Elem='b', setPiece(B, X, Y, 'B', R), setPiece(B, X, Y, P, R)),
								setCounting(C,X,Y,NewCountingBoard),
								setIdentity(Jogador,I,X,Y, NewIdentityBoard),
								showBoard(R), nl,
		
								if_then_else(Move==1, NewMove is Move+1, write('')),
								if_then_else(Move==1, stroke(R,NewCountingBoard,NewIdentityBoard, 1,Counter,NewMove,NewBool,NEWLASTX,NEWLASTY), write('')),

								victory(R,NewCountingBoard,NewIdentityBoard,Jogador),
		
								stroke(R,NewCountingBoard,NewIdentityBoard, 2, Counter, 1, 0, LASTX,LASTY). 
										
putPiece(B,C,I,X, Y, P, Jogador, Counter, Move, Bool, LASTX, LASTY):- 
								Jogador == 2,
								getPiece(B,X,Y,Elem),
								if_then_else(P\='w', replay(B,C,I,Jogador, Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='W', replay(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='B', replay(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='b', replay(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),		
						
								if_then_else( verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY), replay(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								nl,write('X: '), write(X), write('LastX: '), write(LASTX), write('LastY: '), write(LASTY), write('Move: '), write(Move), write('BOOL: '), write(Bool), nl,
								if_then_else(verify1(C,I,X,Y,Jogador,Move,NewBool), NEWLASTX is X, NEWLASTX is 0),
								if_then_else(verify1(C,I,X,Y,Jogador,Move,NewBool), NEWLASTY is Y, NEWLASTY is 0),
								if_then_else(verify2(C,I,X,Y,Jogador,Move,NewBool), replay(B,C,I,Jogador,Counter,Move,NewBool,NEWLASTX,NEWLASTY), write('')),

								if_then_else(Elem='w', setPiece(B, X, Y, 'W', R), setPiece(B, X, Y, P, R)),
								setCounting(C,X,Y,NewCountingBoard),
								setIdentity(Jogador,I,X,Y, NewIdentityBoard),
								showBoard(R), nl,
						
						
								if_then_else(Move==1, NewMove is Move+1, write('')),
								if_then_else(Move==1, stroke(R,NewCountingBoard,NewIdentityBoard, 2,Counter,NewMove,NewBool,NEWLASTX,NEWLASTY), write('')),

		
								stroke(R,NewCountingBoard,NewIdentityBoard, 1, Counter, 1, 0, LASTX,LASTY).



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