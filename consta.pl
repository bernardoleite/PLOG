/*These Functions are responsible to check if it is possible to place a Black Piece 
considering all possible cases of Crosscut*/

verifyCases(Count1,Count2,Count3,Count4,Bool1) :- Count1 = 0, Count2 = 1, Bool1 is 1.
verifyCases(Count1,Count2,Count3,Count4,Bool1) :- Count1 = 0, Count2 = 2, Bool1 is 2.
verifyCases(Count1,Count2,Count3,Count4,Bool1) :- Count1 = 1, Count2 = 2, Bool1 is 3.
verifyCases(Count1,Count2,Count3,Count4,Bool1) :- Count1 = 1, Count2 = 1, Bool1 is 4.



fullCrossCut(C,I,X1,Y1,Jogador,Move,Bool):-   X1>=0, X1=<9, Y1>=0, Y1=<9,
										   getPiece(I,X1,Y1,Elem1),if_then_else(Elem1>0, Elem1=Jogador, write('')),
										   X2 is X1-1, Y2 is Y1+1, getPiece(I,X2,Y2,Elem2), Elem2 = Jogador,
										   X3 is X1, Y3 is Y2, getPiece(I,X3,Y3,Elem3), Elem3 \= 0, Elem3 \= Jogador,
										   X4 is X2, Y4 is Y1, getPiece(I,X4,Y4,Elem4), Elem4 \= 0, Elem4 \= Jogador,
										   getPiece(C,X1,Y1,Count1), getPiece(C,X2,Y2,Count2), getPiece(C,X3,Y3,Count3), getPiece(C,X4,Y4,Count4),
										   Sum1 is Count1+Count2, Sum2 is Count3+Count4,
										   Dif is Sum2-Sum1,
										   Dif=1,
										   verifyCases(Count1,Count2,Count3,Count4,Bool1),
										   if_then_else(Bool1=1, Bool is 11, write('')),
										   if_then_else(Bool1=2, Bool is 21, write('')),
										   if_then_else(Bool1=3, Bool is 31, write('')),
										   if_then_else(Bool1=4, Bool is 41, write('')).
										   
										   
fullCrossCut(C,I,X1,Y1,Jogador,Move,Bool):-   X1>=0, X1=<9, Y1>=0, Y1=<9,
										   getPiece(I,X1,Y1,Elem1), if_then_else(Elem1>0, Elem1=Jogador, write('')),
										   X2 is X1-1, Y2 is Y1-1, getPiece(I,X2,Y2,Elem2), Elem2 = Jogador,
										   X3 is X1, Y3 is Y2, getPiece(I,X3,Y3,Elem3),Elem3 \= 0, Elem3 \= Jogador,
										   X4 is X2, Y4 is Y1, getPiece(I,X4,Y4,Elem4), Elem4 \= 0, Elem4 \= Jogador,
										   getPiece(C,X1,Y1,Count1), getPiece(C,X2,Y2,Count2), getPiece(C,X3,Y3,Count3), getPiece(C,X4,Y4,Count4),
										   Sum1 is Count1+Count2, Sum2 is Count3+Count4,
										   Dif is Sum2-Sum1,
										   Dif=1,
										   verifyCases(Count1,Count2,Count3,Count4,Bool1),
										   if_then_else(Bool1=1, Bool is 12, write('')),
										   if_then_else(Bool1=2, Bool is 22, write('')),
										   if_then_else(Bool1=3, Bool is 32, write('')),
										   if_then_else(Bool1=4, Bool is 42, write('')).
										   
										   
fullCrossCut(C,I,X1,Y1,Jogador,Move,Bool):-   X1>=0, X1=<9, Y1>=0, Y1=<9,
										   getPiece(I,X1,Y1,Elem1),if_then_else(Elem1>0, Elem1=Jogador, write('')),
										   X2 is X1+1, Y2 is Y1-1, getPiece(I,X2,Y2,Elem2), Elem2 = Jogador,
										   X3 is X1, Y3 is Y2, getPiece(I,X3,Y3,Elem3),Elem3 \= 0, Elem3 \= Jogador,
										   X4 is X2, Y4 is Y1, getPiece(I,X4,Y4,Elem4), Elem4 \= 0, Elem4 \= Jogador,
										   getPiece(C,X1,Y1,Count1), getPiece(C,X2,Y2,Count2), getPiece(C,X3,Y3,Count3), getPiece(C,X4,Y4,Count4),
										   Sum1 is Count1+Count2, Sum2 is Count3+Count4,
										   Dif is Sum2-Sum1,
										   Dif=1,
										   verifyCases(Count1,Count2,Count3,Count4,Bool1),
										   if_then_else(Bool1=1, Bool is 13, write('')),
										   if_then_else(Bool1=2, Bool is 23, write('')),
										   if_then_else(Bool1=3, Bool is 33, write('')),
										   if_then_else(Bool1=4, Bool is 43, write('')).
										   
										   
fullCrossCut(C,I,X1,Y1,Jogador,Move,Bool):-   
											X1>=0, X1=<9, Y1>=0, Y1=<9,
										   getPiece(I,X1,Y1,Elem1), if_then_else(Elem1>0, Elem1=Jogador, write('')),
										   X2 is X1+1, Y2 is Y1+1, getPiece(I,X2,Y2,Elem2), Elem2 = Jogador,
										   X3 is X1, Y3 is Y2, getPiece(I,X3,Y3,Elem3),Elem3 \= 0, Elem3 \= Jogador,
										   X4 is X2, Y4 is Y1, getPiece(I,X4,Y4,Elem4), Elem4 \= 0, Elem4 \= Jogador,
										   getPiece(C,X1,Y1,Count1), getPiece(C,X2,Y2,Count2), getPiece(C,X3,Y3,Count3), getPiece(C,X4,Y4,Count4),
										   Sum1 is Count1+Count2, Sum2 is Count3+Count4,
										   Dif is Sum2-Sum1,
										   Dif=1,
										   verifyCases(Count1,Count2,Count3,Count4,Bool1),
										   if_then_else(Bool1=1, Bool is 14, write('')),
										   if_then_else(Bool1=2, Bool is 24, write('')),
										   if_then_else(Bool1=3, Bool is 34, write('')),
										   if_then_else(Bool1=4, Bool is 44, write('')).





/*
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
							 if_then_else(Move=2, Bool is 0, Bool is 1),
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
							 
´
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
							*/
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
									
verifythisCases(Elem1,Elem2,Elem3,Elem4):- Elem1=Elem2, Elem3=0, Elem4=0.
verifythisCases(Elem1,Elem2,Elem3,Elem4):- Elem1=Elem2, Elem3=Jogador, Elem4\=0, Elem4\=Jogador.
verifythisCases(Elem1,Elem2,Elem3,Elem4):- Elem1=Elem2, Elem3\=0, Elem3\=Jogador, Elem4=Jogador.
verifythisCases(Elem1,Elem2,Elem3,Elem4):- Elem1=Elem2, Elem3=Jogador, Elem4=Jogador.
verifythisCases(Elem1,Elem2,Elem3,Elem4):- Elem1=Elem2, Elem3=0, Elem4=Jogador.
verifythisCases(Elem1,Elem2,Elem3,Elem4):- Elem1=Elem2, Elem3=Jogador, Elem4=0.
verifythisCases(Elem1,Elem2,Elem3,Elem4):- Elem1=Elem2, Elem3=0, Elem4\=0, Elem4\=Jogador.
verifythisCases(Elem1,Elem2,Elem3,Elem4):- Elem1=Elem2, Elem3\=0, Elem3\=Jogador, Elem4=0.							

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
									



									
existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2),
									Elem1=Jogador,
									Elem1=Elem2,
									ColumnDifference is Y2-Y1,
									ColumnDifference=1,
									LineDifference is X1-X2,
									LineDifference=1,
									X3 is X1, Y3 is Y2,			
									X4 is X2, Y4 is Y1,
									getPiece(I,X3,Y3,Elem3),
									getPiece(I,X4,Y4,Elem4),
									Elem1\=Elem3, Elem1\=Elem4,
									verifythisCases(Elem1,Elem2,Elem3,Elem4).
									
existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2),
									Elem1=Jogador,
									Elem1=Elem2,
									ColumnDifference is Y1-Y2,
									ColumnDifference=1,
									LineDifference is X1-X2,
									LineDifference=1,
									X3 is X1, Y3 is Y2,
									X4 is X2, Y4 is Y1,
									getPiece(I,X3,Y3,Elem3),
									getPiece(I,X4,Y4,Elem4),
									Elem1\=Elem3, Elem1\=Elem4,
									verifythisCases(Elem1,Elem2,Elem3,Elem4).
									

existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2),
									Elem1=Jogador,
									Elem1=Elem2,
									ColumnDifference is Y1-Y2,
									ColumnDifference=1,
									LineDifference is X2-X1,
									LineDifference=1,
									X3 is X1, Y3 is Y2,
									X4 is X2, Y4 is Y1,
									getPiece(I,X3,Y3,Elem3),
									getPiece(I,X4,Y4,Elem4),
									Elem1\=Elem3, Elem1\=Elem4,
									verifythisCases(Elem1,Elem2,Elem3,Elem4).
									
existsConnection(Jogador,C,I,X1,Y1,X2,Y2):- getPiece(I, X1, Y1, Elem1),
									getPiece(I, X2, Y2, Elem2),
									Elem1=Jogador,
									Elem1=Elem2,
									ColumnDifference is Y2-Y1,
									ColumnDifference=1,
									LineDifference is X2-X1,
									LineDifference=1,
									X3 is X1, Y3 is Y2,
									X4 is X2, Y4 is Y1,
									getPiece(I,X3,Y3,Elem3),
									getPiece(I,X4,Y4,Elem4),
									Elem1\=Elem3, Elem1\=Elem4,
									verifythisCases(Elem1,Elem2,Elem3,Elem4).									

	   
							
							 
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
whiteCoordsRight([[0,9], [1,9], [2,9], [3,9], [4,9],[5,9],[6,9],[7,9],[8,9],[9,9]]).



/*-------------*/

winner(Jogador,Path):-  if_then_else(current_predicate(edge/2), retractall(edge(X,Y)), write('')),
						nl,nl,nl,write('Player '), write(Jogador), write(' is the winner!'), nl,
						write('You made a connected chain!: '), write(Path), menu.
						

search(Jogador,ResultList,Counter):-			
						Counter=<99,
						nth0(Counter,ResultList,Pair),
						nth0(0,Pair,Point1), nth0(1,Pair,Point2),
						if_then_else(path(Point1,Point2,Path), winner(Jogador,Path), write('')),
						NewCounter is Counter+1,
						search(Jogador,ResultList,NewCounter).



agora(Point1,Point2):- assert(edge(Point1,Point2)).

buildEdges(B,C,I,Jogador,Counter,ResultList):- 
						Counter=<4949,
						nth0(Counter,ResultList,Pair),
						nth0(0,Pair,Point1), nth0(1,Pair,Point2),
						nth0(0,Point1,P1x), nth0(1,Point1, P1y),
						nth0(0,Point2,P2x), nth0(1,Point2, P2y), 
						if_then_else(existsConnection(Jogador,C,I,P1x,P1y,P2x,P2y), agora(Point1,Point2), write('')),
						NewCounter is Counter+1,
						buildEdges(B,C,I,Jogador,NewCounter,ResultList).
												
		
findPaths(Jogador):- Jogador==1,
			blackCoordsTop(List1), blackCoordsBottom(List2),
			list_pairs(List1,List2, Pairs),
			if_then_else(current_predicate(edge/2),search(Jogador,Pairs,0),write('')).
			
			
findPaths(Jogador):- Jogador==2,
			whiteCoordsLeft(List1), whiteCoordsRight(List2),
			list_pairs(List1,List2, Pairs),
			if_then_else(current_predicate(edge/2),search(Jogador,Pairs,0),write('')).

			

victory(B,C,I,Jogador):- 
						allCords(ListCoords),
						findall(L, combination(2,ListCoords,L), ResultList),
						if_then_else(buildEdges(B,C,I,Jogador,0,ResultList), write(''), write('')),
						if_then_else(findPaths(Jogador), write(''),write('')),
						if_then_else(current_predicate(edge/2), retractall(edge(X,Y)), write('')).
	
						
							

			
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

				
menu:-  nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
		write('***Welcome to Consta Board Game!***') ,nl,nl,
		write('How do you want to do?'), nl,nl,
		write('Options:'),nl,
		write('(1) -> Human vs Human'),nl,
		write('(2) -> Human vs Computer'),nl,
		write('(3) -> Computer vs Computer'),nl,nl,nl,
		write('Your Option (Select 1, 2 or 3): '), nl,read(Mode),nl,nl,
		write('Select the difficulty: '), nl,nl,
		write('(1) -> Easy'),nl,
		write('(2) -> Moderate'),nl,
		write('(3) -> Hard'),nl,
		write('Your Option (Select 1, 2 or 3): '),nl, read(Dif),nl,nl,
		startGame(Mode,Dif).
		
							 
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==11,  X=LASTX, Y=LASTY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==12,  X=LASTX, Y=LASTY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==13,  X=LASTX, Y=LASTY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==14,  X=LASTX, Y=LASTY.

verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==11, OTHERX is LASTX-1, OTHERY is LASTY+1, X=OTHERX, Y=OTHERY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==12, OTHERX is LASTX-1, OTHERY is LASTY-1, X=OTHERX, Y=OTHERY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==13, OTHERX is LASTX+1, OTHERY is LASTY-1, X=OTHERX, Y=OTHERY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==14, OTHERX is LASTX+1, OTHERY is LASTY+1, X=OTHERX, Y=OTHERY.

verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==21, OTHERX is LASTX, OTHERY is LASTY, X=LASTX, Y=LASTY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==22, OTHERX is LASTX, OTHERY is LASTY, X=LASTX, Y=LASTY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==23, OTHERX is LASTX, OTHERY is LASTY, X=LASTX, Y=LASTY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==24, OTHERX is LASTX, OTHERY is LASTY, X=LASTX, Y=LASTY.

verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==21, OTHERX is LASTX, OTHERY is LASTY, X=OTHERX, Y=OTHERY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==22, OTHERX is LASTX, OTHERY is LASTY, X=OTHERX, Y=OTHERY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==23, OTHERX is LASTX, OTHERY is LASTY, X=OTHERX, Y=OTHERY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==24, OTHERX is LASTX, OTHERY is LASTY, X=OTHERX, Y=OTHERY.
/*
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==41,  X=LASTX, Y=LASTY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==42,  X=LASTX, Y=LASTY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==43,  X=LASTX, Y=LASTY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==44,  X=LASTX, Y=LASTY.
*/
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==41, OTHERX is LASTX-1, OTHERY is LASTY+1, X=OTHERX, Y=OTHERY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==42, OTHERX is LASTX-1, OTHERY is LASTY-1, X=OTHERX, Y=OTHERY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==43, OTHERX is LASTX+1, OTHERY is LASTY-1, X=OTHERX, Y=OTHERY.
verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- Move=2, Bool==44, OTHERX is LASTX+1, OTHERY is LASTY+1, X=OTHERX, Y=OTHERY.


seeIfReplays(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY):- Bool=31. 
seeIfReplays(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY):- Bool=32. 
seeIfReplays(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY):- Bool=33.
seeIfReplays(B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY):- Bool=34.



crossCut(C,I,X,Y,Jogador,Move,NewBool,NEWLASTX, NEWLASTY):-  fullCrossCut(C,I,X,Y,Jogador,Move,Boolean), NewBool is Boolean, NEWLASTX is X, NEWLASTY is Y.

takeActions(LX,LY,LX2,LY2,Mode,Dif,B,C,I,X,Y,Counter,Jogador,Move,NewBool):- if_then_else(Move=2, replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,-10,0,0), write('')).

verifyTheBool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY):- 
				if_then_else(verify_bool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY), 
				write(''), 
				replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY)).

				
startGame(Mode,Dif):-  use_module(library(lists)), use_module(library(system)), use_module(library(random)), initialBoard(B), countingBoard(C), identityBoard(I), showBoard(B), 
					   if_then_else(Mode=1, stroke(LX,LY,LX2,LY2,Mode,Dif,B,C,I,1,1,1,0,0,0), write('')),
					   if_then_else(Mode=2, stroke(LX,LY,LX2,LY2,Mode,Dif,B,C,I,1,1,1,0,0,0), write('')),
					   if_then_else(Mode=3, strokeComputer(LX,LY,LX2,LY2,Mode,Dif,B,C,I,1,1,1,0,0,0), write('')).

modeAndJogador1(Mode,Jogador):- Mode=2, Jogador=1.
modeAndJogador2(Mode,Jogador):- Mode=2, Jogador=2.

replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY):-  write('You cannot put that piece there!'), nl,
																if_then_else(Mode=1, stroke(LX,LY,LX2,LY2,Mode,Dif,B,C,I, Jogador, Counter,Move,Bool,LASTX,LASTY), write('')),
																if_then_else(modeAndJogador1(Mode,Jogador), stroke(LX,LY,LX2,LY2,Mode,Dif,B,C,I, Jogador, Counter,Move,Bool,LASTX,LASTY), write('')),
																if_then_else(modeAndJogador2(Mode,Jogador), strokeComputer(LX,LY,LX2,LY2,Mode,Dif,B,C,I, Jogador, Counter,Move,Bool,LASTX,LASTY), write('')),
																if_then_else(Mode=3, strokeComputer(LX,LY,LX2,LY2,Mode,Dif,B,C,I, Jogador, Counter,Move,Bool,LASTX,LASTY), write('')).
																


stroke(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador, Counter, Move, Bool, LASTX, LASTY):-
								Counter < 100, write('Player '), write(Jogador), write(' it is your turn!'), nl,
								write('Line: '), nl, read(X), nl, 
								write('Column: '), nl, read(Y), nl, 
								write('Piece: '), nl, read(P), nl, 
								write('Nr of Stroke: '), write(Counter), nl, NewCounter is Counter+1, 

								putPiece(LX,LY,LX2,LY2,Mode,Dif,B,C,I, X, Y, P, Jogador, NewCounter, Move,Bool,LASTX,LASTY).

putPiece(LX,LY,LX2,LY2,Mode,Dif,B,C,I, X, Y, P, Jogador, Counter, Move,Bool,LASTX,LASTY):- 
								Jogador == 1,
								getPiece(B,X,Y,Elem),
								if_then_else(P\='b', replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador, Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='B', replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='W', replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='w', replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
	
								if_then_else(Bool>0, verifyTheBool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY), write('')),
								if_then_else(crossCut(C,I,X,Y,Jogador,Move,NewBool,NEWLASTX,NEWLASTY), takeActions(LX,LY,LX2,LY2,Mode,Dif,B,C,I,X,Y,Counter,Jogador,Move,NewBool), NewBool is 0),
								if_then_else(seeIfReplays(B,C,I,Jogador,Counter,Move,NewBool,LASTX,LASTY), replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,-10,LASTX,LASTY), write('')),
								
								if_then_else(Elem='b', setPiece(B, X, Y, 'B', R), setPiece(B, X, Y, P, R)),
								setCounting(C,X,Y,NewCountingBoard),
								setIdentity(Jogador,I,X,Y, NewIdentityBoard),
								showBoard(R), nl,
		
								if_then_else(Move==1, NewMove is Move+1, write('')),
								if_then_else(Move==1, stroke(LX,LY,LX2,LY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard, 1,Counter,NewMove,NewBool,NEWLASTX,NEWLASTY), write('')),

								victory(R,NewCountingBoard,NewIdentityBoard,Jogador),
							
								if_then_else(Mode=1,
											stroke(LX,LY,LX2,LY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard,2,Counter,1,0,LASTX,LASTY),
											write('')
											),

								
								if_then_else(Mode=2,
											strokeComputer(LX,LY,LX2,LY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard,2,Counter,1,0,LASTX,LASTY),
											write('')
											).
											
								
										
putPiece(LX,LY,LX2,LY2,Mode,Dif,B,C,I,X, Y, P, Jogador, Counter, Move, Bool, LASTX, LASTY):- 
								Jogador == 2,
								getPiece(B,X,Y,Elem),
								if_then_else(P\='w', replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador, Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='W', replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='B', replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),
								if_then_else(Elem='b', replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY), write('')),		

								if_then_else(Bool>0, verifyTheBool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY), write('')),
								if_then_else(crossCut(C,I,X,Y,Jogador,Move,NewBool,NEWLASTX,NEWLASTY), takeActions(LX,LY,LX2,LY2,Mode,Dif,B,C,I,X,Y,Counter,Jogador,Move,NewBool), NewBool is 0),
								if_then_else(seeIfReplays(B,C,I,Jogador,Counter,Move,NewBool,LASTX,LASTY), replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,-10,LASTX,LASTY), write('')),

								if_then_else(Elem='w', setPiece(B, X, Y, 'W', R), setPiece(B, X, Y, P, R)),
								setCounting(C,X,Y,NewCountingBoard),
								setIdentity(Jogador,I,X,Y, NewIdentityBoard),
								showBoard(R), nl,
	
								if_then_else(Move==1, NewMove is Move+1, write('')),
								if_then_else(Move==1, stroke(LX,LY,LX2,LY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard, 2,Counter,NewMove,NewBool,NEWLASTX,NEWLASTY), write('')),

		
								victory(R,NewCountingBoard,NewIdentityBoard,Jogador),
		
								stroke(LX,LY,LX2,LY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard, 1, Counter, 1, 0, LASTX,LASTY).


indexOf([Element|_], Element, 0). 
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1), 
  Index is Index1+1. 

 
 generate_board(Length, Board) :-
     length(Board, Length),
     maplist(=([]), Board).
	 
insert_in_list(Item, List, [Item|List]).

 
iterateList(Ger,B,C,I,Jogador,Result,Ite,CordsList):- 
													  Ite < 100,
													  nth0(Ite,CordsList,Point),
												      nth0(0,Point,Px), nth0(1,Point,Py), 
												      getPiece(I, Px, Py, Val), 
												      if_then_else(checkVal(Px,Py,Val,C,Jogador), insert_in_list(Point, Ger, Res), copy(Ger,Res)),										  
													  NewIte is Ite+1,
												      iterateList(Res,B,C,I,Jogador,Result,NewIte,CordsList).										  
												  
checkVal(Px,Py,Val,C,Jogador):- getPiece(C,Px,Py,Valc),
								Val=0, Valc=0.
								
checkVal(Px,Py,Val,C,Jogador):- getPiece(C,Px,Py,Valc),
								Val=Jogador, Valc=1.

								
 iterateList(Ger,B,C,I,Jogador,Result,Ite,CordsList):- Ite == 100, copy(Ger,Result).
 
 possiblePositions(B,C,I,Jogador,Result):-  generate_board(0,Ger),
											allCords(CordsList), 
											if_then_else(iterateList(Ger,B,C,I,Jogador,Result,0,CordsList),write(''),write('')).
								
					

					
					
computerChecksBool(Move,LASTX,LASTY,VALX,VALY,Bool,Pos):-  Bool \= 31, Bool \= 32, Bool \= 33, Bool \= 34, Bool \= 41, Bool \= 42, Bool \= 43, Bool \= 44, Bool>0, VALX is LASTX, VALY is LASTY.
computerChecksBool(Move,LASTX,LASTY,VALX,VALY,Bool,Pos):-  Bool=0, nth0(0,Pos,VALX), nth0(1,Pos,VALY).	
computerChecksBool(Move,LASTX,LASTY,VALX,VALY,Bool,Pos):-  Bool<0, nth0(0,Pos,VALX), nth0(1,Pos,VALY).	
computerChecksBool(Move,LASTX,LASTY,VALX,VALY,Bool,Pos):-  Bool==41, VALX is LASTX-1, VALY is LASTY+1.
computerChecksBool(Move,LASTX,LASTY,VALX,VALY,Bool,Pos):-  Bool==42, VALX is LASTX-1, VALY is LASTY-1.
computerChecksBool(Move,LASTX,LASTY,VALX,VALY,Bool,Pos):-  Bool==43, VALX is LASTX+1, VALY is LASTY-1.
computerChecksBool(Move,LASTX,LASTY,VALX,VALY,Bool,Pos):-  Bool==44, VALX is LASTX+1, VALY is LASTY+1.




	

createRandPos(C,I,Jogador,Move,Bool,LASTX,LASTY,FreePos,Pos,VALX,VALY,Counter):- Counter > 0, member(Pos,FreePos),
																				computerChecksBool(Move,LASTX,LASTY,VALX,VALY,Bool,Pos).
																				 
																				 
					
createRandPos(C,I,Jogador,Move,Bool,LASTX,LASTY,FreePos,Pos,VALX,VALY,Counter):- 
								random(0, 10, AUXX),
								random(0, 10, AUXY),
								NewPos=[AUXX,AUXY],
								NewCounter is Counter+1,
								createRandPos(C,I,Jogador,Move,Bool,LASTX,LASTY,FreePos,NewPos,VALX,VALY,NewCounter).

checkConnection(Jogador,ElemC1,ElemC2):- ElemC1=0, ElemC2=0.											
checkConnection(Jogador,ElemC1,ElemC2):- ElemC1=1, ElemC2=0, ElemI1=Jogador.										
checkConnection(Jogador,ElemC1,ElemC2):-  ElemC1=0, ElemC2=1, ElemI2=Jogador.
												
												
verificance(DIST,Answer):- DIST<1.5, Answer=1.


allEdges(B,C,I,Jogador,Counter,ResultList):- Counter=4950.

allEdges(B,C,I,Jogador,Counter,ResultList):- 
						Counter<4950,
						nth0(Counter,ResultList,Pair),
						nth0(0,Pair,Point1), nth0(1,Pair,Point2),
						nth0(0,Point1,P1x), nth0(1,Point1,P1y),nth0(0,Point2,P2x), nth0(1,Point2,P2y),
						getPiece(C,P1x,P1y,ElemC1),  getPiece(I,P1x,P1y,ElemI1),
						getPiece(C,P2x,P2y,ElemC2),  getPiece(I,P2x,P2y,ElemI2),
						if_then_else(checkConnection(Jogador,ElemC1,ElemC2),Answer is 1, Answer is 0),
						DIFX is P1x-P2x, DIFY is P1y-P2y,
						ABSX is abs(DIFX), ABSY is abs(DIFY),
						QUADX is ABSX*ABSX, QUADY is ABSY*ABSY,
						SUM is QUADX+QUADY,
						DIST is sqrt(SUM), 
						if_then_else(verificance(DIST,Answer), assert(edge(Point1,Point2)), write('')),
						if_then_else(verificance(DIST,Answer), assert(edge(Point2,Point1)), write('')),
						NewCounter is Counter+1,
						allEdges(B,C,I,Jogador,NewCounter,ResultList).
						
						
verifyElemi(ElemI,Point1,Point2,Path):- ElemI=0, path(Point1,Point2,Pathi), copy(Pathi,Path).

tryPairs(B,C,I,Jogador,Pairs,SizePairs,Counter,WAY) :- 
							Counter < SizePairs,
							NewCounter is Counter+1,
							nth0(Counter, Pairs, OnePair),
							nth0(0, OnePair, Point1), nth0(1, OnePair, Point2),
							nth0(0,Point2,P2x), nth0(1,Point2,P2y),
							getPiece(I,P2x,P2y,ElemI), 
							if_then_else(verifyElemi(ElemI,Point1,Point2,Path), copy(Path,WAY), tryPairs(B,C,I,Jogador,Pairs,SizePairs,NewCounter,WAY)).
				
	
searchPath(CurrPoint,B,C,I,Jogador,WAY):-
						Jogador==2,
						allCords(ListCoords),			
						findall(L, combination(2,ListCoords,L), ResultList),
						allEdges(B,C,I,Jogador,0,ResultList),
						ThisPoint=[CurrPoint],
						whiteCoordsRight(ListWhiteRight),
						list_pairs(ThisPoint, ListWhiteRight, Pairs),
						length(Pairs,SizePairs),
						if_then_else(tryPairs(B,C,I,Jogador,Pairs,SizePairs,0,WAY), write(''), write('')),
						retractall(edge(X,Y)).
						
						
searchPath(CurrPoint,B,C,I,Jogador,WAY):-
						Jogador==1,
						allCords(ListCoords),			
						findall(L, combination(2,ListCoords,L), ResultList),
						allEdges(B,C,I,Jogador,0,ResultList),
						ThisPoint=[CurrPoint],
						blackCoordsBottom(ListBlackBottom),
						list_pairs(ThisPoint, ListBlackBottom, Pairs),
						length(Pairs,SizePairs),
						if_then_else(tryPairs(B,C,I,Jogador,Pairs,SizePairs,0,WAY), write(''), write('')),
						retractall(edge(X,Y)).
						

verifyStroke3(Jogador,Mode,Dif,VALX,VALY,Counter,CurrPoint):- Jogador=1, Mode=3, Dif=2, Counter=1, VALX is 0, VALY is 7, CurrPoint=[0,7].	
					
verifyStroke3(Jogador,Mode,Dif,VALX,VALY,Counter,CurrPoint):- Jogador=2, Mode=2, Dif=2, Counter=3, VALX is 5, VALY is 0, CurrPoint=[5,0].
verifyStroke3(Jogador,Mode,Dif,VALX,VALY,Counter,CurrPoint):- Jogador=2, Mode=3, Dif=2, Counter=3, VALX is 5, VALY is 0, CurrPoint=[5,0].


verifyStroke(Jogador,Mode,Dif,VALX,VALY,Counter,WAY):- Jogador = 1, Mode=3, Dif=2, Counter>1, is_list(WAY), nth0(1,WAY,Point), nth0(0,Point,VALX),nth0(1,Point,VALY).

verifyStroke(Jogador,Mode,Dif,VALX,VALY,Counter,WAY):- Jogador = 2, Mode=2, Dif=2, Counter>3, is_list(WAY), nth0(1,WAY,Point), nth0(0,Point,VALX),nth0(1,Point,VALY).
verifyStroke(Jogador,Mode,Dif,VALX,VALY,Counter,WAY):- Jogador = 2, Mode=3, Dif=2, Counter>3, is_list(WAY), nth0(1,WAY,Point), nth0(0,Point,VALX),nth0(1,Point,VALY).


atrbL(Jogador,LX,LY,LX2,LY2,CurrPoint):- Jogador=1, CurrPoint=[LX2,LY2].
atrbL(Jogador,LX,LY,LX2,LY2,CurrPoint):- Jogador=2, CurrPoint=[LX,LY].
							
strokeComputer(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,Bool,LASTX,LASTY):-
								Counter < 1000, write('Player '), write(Jogador), write('(Computer) '), write(' it is your turn!'), nl,
								possiblePositions(B,C,I,Jogador,FreePos), 
								
								if_then_else(Dif==1, createRandPos(C,I,Jogador,Move,Bool,LASTX,LASTY,FreePos,Pos,VALX,VALY,0), write('')),
								
						
		
								if_then_else(verifyStroke3(Jogador,Mode,Dif,VALX,VALY,Counter,CurrPoint),write(''),atrbL(Jogador,LX,LY,LX2,LY2,CurrPoint)),
								
						
								
								if_then_else(Dif==2, searchPath(CurrPoint,B,C,I,Jogador,WAY), write('')),						

								if_then_else(verifyStroke(Jogador,Mode,Dif,VALX,VALY,Counter,WAY),write(WAY),write('')),

					
								
								write('Line: '), nl,  write(VALX), sleep(0), nl, 
								write('Column: '), nl, write(VALY),sleep(0), nl, 
								write('Piece: '), nl, if_then_else(Jogador==1,write('b'),write('w')), nl, sleep(0),
								if_then_else(Jogador==1, P = 'b', P = 'w'),
								write('Nr of Stroke: '), write(Counter), nl, NewCounter is Counter+1, 
								putPieceComputer(LX,LY,LX2,LY2,Mode,Dif,B,C,I,VALX,VALY,P,Jogador,NewCounter,Move,Bool,LASTX,LASTY).	


moveAndMode(Move,Mode):- Move==1, Mode==3.
								
putPieceComputer(LX,LY,LX2,LY2,Mode,Dif,B,C,I, X, Y, P, Jogador, Counter, Move,Bool,LASTX,LASTY):- 
								Jogador == 1,
								getPiece(B,X,Y,Elem),

	
								if_then_else(Bool>0, verifyTheBool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY), write('')),
								if_then_else(crossCut(C,I,X,Y,Jogador,Move,NewBool,NEWLASTX,NEWLASTY), takeActions(LX,LY,LX2,LY2,Mode,Dif,B,C,I,X,Y,Counter,Jogador,Move,NewBool), NewBool is 0),
								if_then_else(seeIfReplays(B,C,I,Jogador,Counter,Move,NewBool,LASTX,LASTY), replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,-10,LASTX,LASTY), write('')),
								
								if_then_else(Elem='b', setPiece(B, X, Y, 'B', R), setPiece(B, X, Y, P, R)),
								setCounting(C,X,Y,NewCountingBoard),
								setIdentity(Jogador,I,X,Y, NewIdentityBoard),
								showBoard(R), nl,
								if_then_else(Move==1, NewMove is Move+1, write('')),
								
								
								NEWLX2 is X, NEWLY2 is Y,
						
								
								if_then_else(moveAndMode(Move,Mode), strokeComputer(LX,LY,NEWLX2,NEWLY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard, 1,Counter,NewMove,NewBool,NEWLASTX,NEWLASTY), write('')),
								if_then_else(Move==1, stroke(LX,LY,NEWLX2,NEWLY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard, 1,Counter,NewMove,NewBool,NEWLASTX,NEWLASTY), write('')),


								victory(R,NewCountingBoard,NewIdentityBoard,Jogador),


								
								strokeComputer(LX,LY,NEWLX2,NEWLY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard,2,Counter,1,0,LASTX,LASTY).

											
											
											
											
putPieceComputer(LX,LY,LX2,LY2,Mode,Dif,B,C,I,X,Y,P,Jogador,Counter,Move,Bool,LASTX,LASTY):- 
								Jogador == 2,
								getPiece(B,X,Y,Elem),	

								if_then_else(Bool>0, verifyTheBool(B,C,I,Jogador,Counter,Move,Bool,X,Y,LASTX,LASTY), write('')),
								if_then_else(crossCut(C,I,X,Y,Jogador,Move,NewBool,NEWLASTX,NEWLASTY), takeActions(LX,LY,LX2,LY2,Mode,Dif,B,C,I,X,Y,Counter,Jogador,Move,NewBool), NewBool is 0),
								if_then_else(seeIfReplays(B,C,I,Jogador,Counter,Move,NewBool,LASTX,LASTY), replay(LX,LY,LX2,LY2,Mode,Dif,B,C,I,Jogador,Counter,Move,-10,LASTX,LASTY), write('')),

								if_then_else(Elem='w', setPiece(B, X, Y, 'W', R), setPiece(B, X, Y, P, R)),
								
								setCounting(C,X,Y,NewCountingBoard),
								setIdentity(Jogador,I,X,Y, NewIdentityBoard),
								showBoard(R), nl,

								NEWLX is X, NEWLY is Y,
							
								if_then_else(Move==1, NewMove is Move+1, write('')),
								if_then_else(Move==1, strokeComputer(NEWLX,NEWLY,LX2,LY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard, 2,Counter,NewMove,NewBool,NEWLASTX,NEWLASTY), write('')),

		
								victory(R,NewCountingBoard,NewIdentityBoard,Jogador),
								
								

								if_then_else(Mode=1,
											stroke(NEWLX,NEWLY,LX2,LY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard,1,Counter,1,0,LASTX,LASTY),
											write('')
											),

								if_then_else(Mode=2,
											stroke(NEWLX,NEWLY,LX2,LY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard,1,Counter,1,0,LASTX,LASTY),
											write('')
											),
											

								if_then_else(Mode=3,
											strokeComputer(NEWLX,NEWLY,LX2,LY2,Mode,Dif,R,NewCountingBoard,NewIdentityBoard,1,Counter,1,0,LASTX,LASTY),
											write('')
											).
											
								

								

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