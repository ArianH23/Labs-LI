nat(0).
nat(N) :- nat(N1), N is N1 + 1.

%Desplazamientos hacia la Derecha
cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  0):- M1x is M1-1,M2x is M2+1, C1x is C1, C2x is C2, M1 > 0.
cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  0):- C1x is C1-1,C2x is C2+1, M1x is M1, M2x is M2, C1 > 0.

cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  0):- M1x is M1-2,M2x is M2+2, C1x is C1, C2x is C2,M1 > 1.
cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  0):- C1x is C1-2,C2x is C2+2, M1x is M1, M2x is M2,C1 > 1.

cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  0):- M1x is M1-1,M2x is M2+1, C1x is C1-1,C2x is C2+1, M1 > 0, C1 > 0.


%Desplazamientos hacia la izquierda
cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  1):- M2x is M2-1,M1x is M1+1, C1x is C1, C2x is C2, M2 > 0.
cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  1):- C2x is C2-1,C1x is C1+1, M1x is M1, M2x is M2, C2 > 0.

cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  1):- M2x is M2-2,M1x is M1+2,C1x is C1, C2x is C2, M1 > 1.
cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  1):- C2x is C2-2,C1x is C1+2,M1x is M1, M2x is M2, C1 > 1.

cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,  1):- M2x is M2-1,M1x is M1+1,C2x is C2-1,C1x is C1+1, M2 > 0, C2 > 0.


unPaso([[M1,C1],[M2,C2],Lado1],[[M1x,C1x],[M2x,C2x],Lado2]):- 
            Lado2 is 1-Lado1,
            cambiaLado(M1,C1,M2,C2,    M1x,C1x,M2x,C2x,    Lado1),C1x>=0,C2x>=0,M1x>=0,M2x>=0, 
            ((M1x is 0,C1x>=0,M2x>=C2x);(M2x is 0,C2x >=0,M1x>=C1x);(M1x>=C1x,M2x>=C2x))% se pueden suponer cosas teniendo en cuenta que M1x o M2x es 0, pero asi queda más claro
            .


camino(E,E, C,C).
camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal) :-
    unPaso(EstadoActual, EstSiguiente),
    \+member(EstSiguiente, CaminoHastaAhora),
    camino(EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal).

solucionOptima :-
    nat(N),
    camino([[3,3],[0,0],0], [[0,0],[3,3],1], [[[3,3],[0,0],0]], C),
    length(C, N),
	write('Coste: '), write(N), nl,
    reverse(C, I),
    write(I).