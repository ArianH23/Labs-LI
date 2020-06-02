programa(P) :-
    append([begin|L], [end], P),
    instrucciones(L).

instrucciones(I) :-
    instruccion(I).

instrucciones(I) :-
    append([X, [;], Y], I),
    instruccion(X),
    instrucciones(Y).

instruccion([V, =, X, +, Y]) :-
    variable(V),
    variable(X),
    variable(Y).

instruccion([if, V, =, X, then|L]) :-
    variable(V),
    variable(X),
    append([L1, [else], L2, [endif]], L),
    instrucciones(L1),
    instrucciones(L2).

variable(x).
variable(y).
variable(z).