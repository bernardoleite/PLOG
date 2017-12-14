:-use_module(library(clpfd)), use_module(library(lists)).

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

	
teste(L):-

%1-Laranja 2-Azul 3-Verde 4-Amarelo, 5-Rosa

%A definir pelo utilizador.......

Penta1=[1,2,5,4,3],
Penta2=[1,5,3,2,4],
Penta3=[4,5,1,3,2],
Penta4=[4,3,5,2,1],
Penta5=[2,3,4,1,5],
Penta6=[1,2,3,4,5],
Penta7=[3,5,4,2,1],
Penta8=[4,1,3,2,5],
Penta9=[3,2,1,5,4],
Penta10=[1,4,2,5,3],
Penta11=[2,5,1,4,3],
Penta12=[4,5,3,2,1],

Pentagons=[Penta1,Penta2,Penta3,Penta4,Penta5,Penta6,Penta7,Penta8,Penta9,Penta10,Penta11,Penta12],


length(Pentagons,Tam),
generateAllPentagons(Pentagons,Tam,[],AllPenta,0),


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


A1#=1 #/\ A2#=2 #/\ A3#=3 #/\ A4#=4 #/\ A5#=5 #/\

B1#=3 #/\ B2#=4 #/\ B3#=1 #/\ B4#=5 #/\ B5#=2 #/\

C1#=3 #/\ C2#=1 #/\ C3#=5 #/\ C4#=4 #/\ C5#=2 #/\

D1#=3 #/\ D2#=5 #/\ D3#=4 #/\ D4#=1 #/\ D5#=2 #/\

E1#=4 #/\ E2#=1 #/\ E3#=3 #/\ E4#=5 #/\ E5#=2 #/\

F1#=4 #/\ F2#=5 #/\ F3#=2 #/\ F4#=1 #/\ F5#=3 #/\

G1#=2 #/\ G2#=1 #/\ G3#=3 #/\ G4#=5 #/\ G5#=4 #/\

H1#=2 #/\ H2#=5 #/\ H3#=4 #/\ H4#=1 #/\ H5#=3 #/\

I1#=2 #/\ I2#=4 #/\ I3#=1 #/\ I4#=5 #/\ I5#=3 #/\

J1#=2 #/\ J2#=1 #/\ J3#=5 #/\ J4#=4 #/\ J5#=3 #/\



A1#=B3 #/\ A2#=F3 #/\ A3#=E3 #/\ A4#=D3 #/\ A5#=C3 #/\

B1#=H5 #/\ B2#=C4 #/\ B3#=A1 #/\ B4#=F2 #/\ B5#=G1 #/\

C1#=I5 #/\ C2#=D4 #/\ C3#=A5 #/\ C4#=B2 #/\ C5#=H1 #/\

D1#=J5 #/\ D2#=E4 #/\ D3#=A4 #/\ D4#=C2 #/\ D5#=I1 #/\

E1#=L5 #/\ E2#=F4 #/\ E3#=A3 #/\ E4#=D2 #/\ E5#=J1 #/\

F1#=G5 #/\ F2#=B4 #/\ F3#=A2 #/\ F4#=E2 #/\ F5#=L1 #/\

G1#=B5 #/\ G2#=H4 #/\ G3#=M4 #/\ G4#=L2 #/\ G5#=F1 #/\

H1#=C5 #/\ H2#=I4 #/\ H3#=M3 #/\ H4#=G2 #/\ H5#=B1 #/\

I1#=D5 #/\ I2#=J4 #/\ I3#=M2 #/\ I4#=H2 #/\ I5#=C1 #/\

J1#=E5 #/\ J2#=L4 #/\ J3#=M1 #/\ J4#=I2 #/\ J5#=D1 #/\

L1#=F5 #/\ L2#=G4 #/\ L3#=M5 #/\ L4#=J2 #/\ L5#=E1 #/\

M1#=J3 #/\ M2#=I3 #/\ M3#=H3 #/\ M4#=G3 #/\ M5#=L3,

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


%Face1
Face1_2=[A2,A3,A4,A5,A1], Face1_3=[A3,A4,A5,A1,A2], Face1_4=[A4,A5,A1,A2,A3], Face1_5=[A5,A1,A2,A3,A4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1_2,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1_3,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1_4,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1_5,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),

%Face2
Face2_2=[B2,B3,B4,B5,B1], Face2_3=[B3,B4,B5,B1,B2], Face2_4=[B4,B5,B1,B2,B3], Face2_5=[B5,B1,B2,B3,B4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2_2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2_3,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2_4,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2_5,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),

%Face3
Face3_2=[C2,C3,C4,C5,C1], Face3_3=[C3,C4,C5,C1,C2], Face3_4=[C4,C5,C1,C2,C3], Face3_5=[C5,C1,C2,C3,C4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3_2,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3_3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3_4,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3_5,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),

%Face4
Face4_2=[D2,D3,D4,D5,D1], Face4_3=[D3,D4,D5,D1,D2], Face4_4=[D4,D5,D1,D2,D3], Face4_5=[D5,D1,D2,D3,D4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4_2,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4_3,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4_4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4_5,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),

%Face5
Face5_2=[E2,E3,E4,E5,E1], Face5_3=[E3,E4,E5,E1,E2], Face5_4=[E4,E5,E1,E2,E3], Face5_5=[E5,E1,E2,E3,E4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5_2,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5_3,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5_4,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5_5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),

%Face6
Face6_2=[F2,F3,F4,F5,F1], Face6_3=[F3,F4,F5,F1,F2], Face6_4=[F4,F5,F1,F2,F3], Face6_5=[F5,F1,F2,F3,F4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6_2,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6_3,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6_4,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6_5,Face7,Face8,Face9,Face10,Face11,Face12]),

%Face7
Face7_2=[G2,G3,G4,G5,G1], Face7_3=[G3,G4,G5,G1,G2], Face7_4=[G4,G5,G1,G2,G3], Face7_5=[G5,G1,G2,G3,G4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7_2,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7_3,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7_4,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7_5,Face8,Face9,Face10,Face11,Face12]),

%Face8
Face8_2=[H2,H3,H4,H5,H1], Face8_3=[H3,H4,H5,H1,H2], Face8_4=[H4,H5,H1,H2,H3], Face8_5=[H5,H1,H2,H3,H4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8_2,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8_3,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8_4,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8_5,Face9,Face10,Face11,Face12]),

%Face9
Face9_2=[I2,I3,I4,I5,I1], Face9_3=[I3,I4,I5,I1,I2], Face9_4=[I4,I5,I1,I2,I3], Face9_5=[I5,I1,I2,I3,I4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9_2,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9_3,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9_4,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9_5,Face10,Face11,Face12]),

%Face10
Face10_2=[J2,J3,J4,J5,J1], Face10_3=[J3,J4,J5,J1,J2], Face10_4=[J4,J5,J1,J2,J3], Face10_5=[J5,J1,J2,J3,J4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10_2,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10_3,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10_4,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10_5,Face11,Face12]),

%Face11
Face11_2=[L2,L3,L4,L5,L1], Face11_3=[L3,L4,L5,L1,L2], Face11_4=[L4,L5,L1,L2,L3], Face11_5=[L5,L1,L2,L3,L4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11_2,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11_3Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11_4,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11_5,Face12]),

%Face12
Face12_2=[M2,M3,M4,M5,M1], Face12_3=[M3,M4,M5,M1,M2], Face12_4=[M4,M5,M1,M2,M3], Face12_5=[M5,M1,M2,M3,M4],
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12_2]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12_3]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12_4]),
allListsDifferent([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12_5]),


append([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12],L),

labeling([], L),

write(Face1),nl,
write(Face2),nl,
write(Face3),nl,
write(Face4),nl,
write(Face5),nl,
write(Face6),nl,
write(Face7),nl,
write(Face8),nl,
write(Face9),nl,
write(Face10),nl,
write(Face11),nl,
write(Face12),nl.