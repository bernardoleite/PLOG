dodekduo_Puzzle2_Gen(Solution,MinFigures,MaxFigures):-

%1-Retangulo 2-Semi-Circulo 3-Triangulo

PentaA=[PentaA1,PentaA2,PentaA3,PentaA4,PentaA5], 
PentaB=[PentaB1,PentaB2,PentaB3,PentaB4,PentaB5], 
PentaC=[PentaC1,PentaC2,PentaC3,PentaC4,PentaC5],
PentaD=[PentaD1,PentaD2,PentaD3,PentaD4,PentaD5], 
PentaE=[PentaE1,PentaE2,PentaE3,PentaE4,PentaE5], 
PentaF=[PentaF1,PentaF2,PentaF3,PentaF4,PentaF5], 
PentaG=[PentaG1,PentaG2,PentaG3,PentaG4,PentaG5], 
PentaH=[PentaH1,PentaH2,PentaH3,PentaH4,PentaH5], 
PentaI=[PentaI1,PentaI2,PentaI3,PentaI4,PentaI5], 
PentaJ=[PentaJ1,PentaJ2,PentaJ3,PentaJ4,PentaJ5], 
PentaL=[PentaL1,PentaL2,PentaL3,PentaL4,PentaL5], 
PentaM=[PentaM1,PentaM2,PentaM3,PentaM4,PentaM5], 

domain(PentaA,MinFigures,MaxFigures),
domain(PentaB,MinFigures,MaxFigures),
domain(PentaC,MinFigures,MaxFigures),
domain(PentaD,MinFigures,MaxFigures),
domain(PentaE,MinFigures,MaxFigures),
domain(PentaF,MinFigures,MaxFigures),
domain(PentaG,MinFigures,MaxFigures),
domain(PentaH,MinFigures,MaxFigures),
domain(PentaI,MinFigures,MaxFigures),
domain(PentaJ,MinFigures,MaxFigures),
domain(PentaL,MinFigures,MaxFigures),
domain(PentaM,MinFigures,MaxFigures),

Pentagons=[PentaA,PentaB,PentaC,PentaD,PentaE,PentaF,PentaG,PentaH,PentaI,PentaJ,PentaL,PentaM],
generateAllPentagons(Pentagons,12,[],AllPenta,0),
allListsDifferent(AllPenta),

fd_dom(PentaA1,Penta_A1),fd_dom(PentaA2,Penta_A2),fd_dom(PentaA3,Penta_A3),fd_dom(PentaA4,Penta_A4),fd_dom(PentaA5,Penta_A5),
fd_dom(PentaB1,Penta_B1),fd_dom(PentaB2,Penta_B2),fd_dom(PentaB3,Penta_B3),fd_dom(PentaB4,Penta_B4),fd_dom(PentaB5,Penta_B5),
fd_dom(PentaC1,Penta_C1),fd_dom(PentaC2,Penta_C2),fd_dom(PentaC3,Penta_C3),fd_dom(PentaC4,Penta_C4),fd_dom(PentaC5,Penta_C5),
fd_dom(PentaD1,Penta_D1),fd_dom(PentaD2,Penta_D2),fd_dom(PentaD3,Penta_D3),fd_dom(PentaD4,Penta_D4),fd_dom(PentaD5,Penta_D5),
fd_dom(PentaE1,Penta_E1),fd_dom(PentaE2,Penta_E2),fd_dom(PentaE3,Penta_E3),fd_dom(PentaE4,Penta_E4),fd_dom(PentaE5,Penta_E5),
fd_dom(PentaF1,Penta_F1),fd_dom(PentaF2,Penta_F2),fd_dom(PentaF3,Penta_F3),fd_dom(PentaF4,Penta_F4),fd_dom(PentaF5,Penta_F5),
fd_dom(PentaG1,Penta_G1),fd_dom(PentaG2,Penta_G2),fd_dom(PentaG3,Penta_G3),fd_dom(PentaG4,Penta_G4),fd_dom(PentaG5,Penta_G5),
fd_dom(PentaH1,Penta_H1),fd_dom(PentaH2,Penta_H2),fd_dom(PentaH3,Penta_H3),fd_dom(PentaH4,Penta_H4),fd_dom(PentaH5,Penta_H5),
fd_dom(PentaI1,Penta_I1),fd_dom(PentaI2,Penta_I2),fd_dom(PentaI3,Penta_I3),fd_dom(PentaI4,Penta_I4),fd_dom(PentaI5,Penta_I5),
fd_dom(PentaJ1,Penta_J1),fd_dom(PentaJ2,Penta_J2),fd_dom(PentaJ3,Penta_J3),fd_dom(PentaJ4,Penta_J4),fd_dom(PentaJ5,Penta_J5),
fd_dom(PentaL1,Penta_L1),fd_dom(PentaL2,Penta_L2),fd_dom(PentaL3,Penta_L3),fd_dom(PentaL4,Penta_L4),fd_dom(PentaL5,Penta_L5),
fd_dom(PentaM1,Penta_M1),fd_dom(PentaM2,Penta_M2),fd_dom(PentaM3,Penta_M3),fd_dom(PentaM4,Penta_M4),fd_dom(PentaM5,Penta_M5),

Penta_A=[Penta_A1,Penta_A2,Penta_A3,Penta_A4,Penta_A5], 
Penta_B=[Penta_B1,Penta_B2,Penta_B3,Penta_B4,Penta_B5], 
Penta_C=[Penta_C1,Penta_C2,Penta_C3,Penta_C4,Penta_C5], 
Penta_D=[Penta_D1,Penta_D2,Penta_D3,Penta_D4,Penta_D5],
Penta_E=[Penta_E1,Penta_E2,Penta_E3,Penta_E4,Penta_E5], 
Penta_F=[Penta_F1,Penta_F2,Penta_F3,Penta_F4,Penta_F5], 
Penta_G=[Penta_G1,Penta_G2,Penta_G3,Penta_G4,Penta_G5], 
Penta_H=[Penta_H1,Penta_H2,Penta_H3,Penta_H4,Penta_H5], 
Penta_I=[Penta_I1,Penta_I2,Penta_I3,Penta_I4,Penta_I5], 
Penta_J=[Penta_J1,Penta_J2,Penta_J3,Penta_J4,Penta_J5], 
Penta_L=[Penta_L1,Penta_L2,Penta_L3,Penta_L4,Penta_L5], 
Penta_M=[Penta_M1,Penta_M2,Penta_M3,Penta_M4,Penta_M5],  

Pentagons_VAR=[Penta_A,Penta_B,Penta_C,Penta_D,Penta_E,Penta_F,Penta_G,Penta_H,Penta_I,Penta_J,Penta_L,Penta_M],
generateAllPentagons(Pentagons_VAR,12,[],AllPenta_VAR,0),

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

%Applying domain from 1 to MaxFigures refering to the number of Figures that compose a face

domain(Face1,MinFigures,MaxFigures),
domain(Face2,MinFigures,MaxFigures),
domain(Face3,MinFigures,MaxFigures),
domain(Face4,MinFigures,MaxFigures),
domain(Face5,MinFigures,MaxFigures),
domain(Face6,MinFigures,MaxFigures),
domain(Face7,MinFigures,MaxFigures),
domain(Face8,MinFigures,MaxFigures),
domain(Face9,MinFigures,MaxFigures),
domain(Face10,MinFigures,MaxFigures),
domain(Face11,MinFigures,MaxFigures),
domain(Face12,MinFigures,MaxFigures),

table([Face1],Pentagons_VAR),
table([Face2],Pentagons_VAR),
table([Face3],Pentagons_VAR),
table([Face4],Pentagons_VAR),
table([Face5],Pentagons_VAR),
table([Face7],Pentagons_VAR),
table([Face8],Pentagons_VAR),
table([Face9],Pentagons_VAR),
table([Face10],Pentagons_VAR),
table([Face11],Pentagons_VAR),
table([Face12],Pentagons_VAR),

fd_dom(A1,Face_A1),fd_dom(A2,Face_A2),fd_dom(A3,Face_A3),fd_dom(A4,Face_A4),fd_dom(A5,Face_A5),
fd_dom(B1,Face_B1),fd_dom(B2,Face_B2),fd_dom(B3,Face_B3),fd_dom(B4,Face_B4),fd_dom(B5,Face_B5),
fd_dom(C1,Face_C1),fd_dom(C2,Face_C2),fd_dom(C3,Face_C3),fd_dom(C4,Face_C4),fd_dom(C5,Face_C5),
fd_dom(D1,Face_D1),fd_dom(D2,Face_D2),fd_dom(D3,Face_D3),fd_dom(D4,Face_D4),fd_dom(D5,Face_D5),
fd_dom(E1,Face_E1),fd_dom(E2,Face_E2),fd_dom(E3,Face_E3),fd_dom(E4,Face_E4),fd_dom(E5,Face_E5),
fd_dom(F1,Face_F1),fd_dom(F2,Face_F2),fd_dom(F3,Face_F3),fd_dom(F4,Face_F4),fd_dom(F5,Face_F5),
fd_dom(G1,Face_G1),fd_dom(G2,Face_G2),fd_dom(G3,Face_G3),fd_dom(G4,Face_G4),fd_dom(G5,Face_G5),
fd_dom(H1,Face_H1),fd_dom(H2,Face_H2),fd_dom(H3,Face_H3),fd_dom(H4,Face_H4),fd_dom(H5,Face_H5),
fd_dom(I1,Face_I1),fd_dom(I2,Face_I2),fd_dom(I3,Face_I3),fd_dom(I4,Face_I4),fd_dom(I5,Face_I5),
fd_dom(J1,Face_J1),fd_dom(J2,Face_J2),fd_dom(J3,Face_J3),fd_dom(J4,Face_J4),fd_dom(J5,Face_J5),
fd_dom(L1,Face_L1),fd_dom(L2,Face_L2),fd_dom(L3,Face_L3),fd_dom(L4,Face_L4),fd_dom(L5,Face_L5),
fd_dom(M1,Face_M1),fd_dom(M2,Face_M2),fd_dom(M3,Face_M3),fd_dom(M4,Face_M4),fd_dom(M5,Face_M5),

Face_A=[Face_A1,Face_A2,Face_A3,Face_A4,Face_A5], 
Face_B=[Face_B1,Face_B2,Face_B3,Face_B4,Face_B5], 
Face_C=[Face_C1,Face_C2,Face_C3,Face_C4,Face_C5], 
Face_D=[Face_D1,Face_D2,Face_D3,Face_D4,Face_D5],
Face_E=[Face_E1,Face_E2,Face_E3,Face_E4,Face_E5], 
Face_F=[Face_F1,Face_F2,Face_F3,Face_F4,Face_F5], 
Face_G=[Face_G1,Face_G2,Face_G3,Face_G4,Face_G5], 
Face_H=[Face_H1,Face_H2,Face_H3,Face_H4,Face_H5], 
Face_I=[Face_I1,Face_I2,Face_I3,Face_I4,Face_I5], 
Face_J=[Face_J1,Face_J2,Face_J3,Face_J4,Face_J5], 
Face_L=[Face_L1,Face_L2,Face_L3,Face_L4,Face_L5], 
Face_M=[Face_M1,Face_M2,Face_M3,Face_M4,Face_M5], 

Faces_VAR=[Face_A,Face_B,Face_C,Face_D,Face_E,Face_F,Face_G,Face_H,Face_I,Face_J,Face_L,Face_M],
generateAllPentagons(Faces_VAR,12,[],AllFaces_VAR,0), 


table([PentaA],AllFaces_VAR),
table([PentaB],AllFaces_VAR),
table([PentaC],AllFaces_VAR),
table([PentaD],AllFaces_VAR),
table([PentaE],AllFaces_VAR),
table([PentaF],AllFaces_VAR),
table([PentaG],AllFaces_VAR),
table([PentaH],AllFaces_VAR),
table([PentaI],AllFaces_VAR),
table([PentaJ],AllFaces_VAR),
table([PentaL],AllFaces_VAR),
table([PentaM],AllFaces_VAR),

%Two faces that are adjacent must have the same figure

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


%Check if faces are different, including Rotations

AllFaces=[Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12],

append(AllFaces,AllElements),
MaxFigures2 is MaxFigures+1,
forceShapes(AllFaces,MaxFigures2,1),

generateAllPentagons(AllFaces,12,[],AllRots,0),

allListsDifferent(AllRots),

append([Face1,Face2,Face3,Face4,Face5,Face6,Face7,Face8,Face9,Face10,Face11,Face12],Solution),
labeling([], Solution),

generateProb(AllFaces,Problem),

if_then_else(dodekduo_Puzzle2_Sol(NewSolution,Problem,MaxFigures,1),continueplay,(nl,nl,write('No More Solutions!!!'),nl,nl)),

write('-> Generate other Problem? (Type 1 for YES and 2 for NO)'),nl,
write('Your Option (Select 1 or 2): '), nl, read(Option1),nl,nl,

if_then_else(Option1=1,fail, menu).