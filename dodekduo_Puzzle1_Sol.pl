dodekduo_Puzzle1_Sol(Solution):-

%1-Laranja 2-Azul 3-Verde 4-Amarelo, 5-Rosa

Penta1=[1,2,5,4,3],Penta2=[1,5,3,2,4],Penta3=[4,5,1,3,2],
Penta4=[4,3,5,2,1],Penta5=[2,3,4,1,5],Penta6=[1,2,3,4,5],
Penta7=[3,5,4,2,1],Penta8=[4,1,3,2,5],Penta9=[3,2,1,5,4],
Penta10=[1,4,2,5,3],Penta11=[2,5,1,4,3],Penta12=[4,5,3,2,1],

Pentagons=[Penta1,Penta2,Penta3,Penta4,Penta5,Penta6,Penta7,Penta8,Penta9,Penta10,Penta11,Penta12],

length(Pentagons,Tam), 
generateAllPentagons(Pentagons,Tam,[],AllPenta,0),

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


		%Nota: Pelo facto do Dodecaedro ter 60 simetrias de rotação,
		%esta restrição faz com que a solução corresponda à do enunciado.
		A1#=1 #/\ A2#=2 #/\ A3#=3 #/\ A4#=4 #/\ A5#=5 #/\

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
findall(Solution,labeling([], Solution),AllSol),

write('**********Problem Input: 12 Pentagons**********'), nl,nl,
printProblem(Pentagons), nl , nl,

write('-> Note: Because there are 60 rotational symmetries '),nl,
write('in a regular dodecahedron there are (1 solution * 60) total solutions to this problem.'),nl,nl,
write('-> The solution presented corresponds to the problem statement.'),nl,nl,

write('**********Solution:**********'), nl,nl,
printSolution(Pentagons,Solution).