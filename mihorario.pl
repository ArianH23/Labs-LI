symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.


%%%%%%%%%%%%%%%%%%%%% toy input example:
:-include(entradaHoraris3).

%%%%%% Some helpful definitions to make the code cleaner:
year(Y)                 :- numCursos(N),between(1,N,Y).
course(C)               :- numAssignatures(N),between(1,N,C).
room(R)                 :- numAules(N),between(1,N,R).
teacher(T)              :- numProfes(N),between(1,N,T).
hour(H)                 :- between(1,12,H).
day(D)                  :- between(1,5,D).


lectureOfCourse(C,L)    :- assig(_,C,N,_,_),between(1,N,L).
slot(S)                 :- between(1,60,S).
slotOfDay(D,S)          :- hour(H), S is (D-1)*12 + H.
courseOfYear(Y,C)       :- assig(Y,C,_,_,_).



%%%%%%  1. SAT Variables:                                          % Meanings:

satVariable( cls(C,L,S) ):- course(C), lectureOfCourse(C,L),slot(S).
satVariable( cr(C,R) ):- course(C), room(R).
satVariable( ct(C,T) ):- course(C),teacher(T).
satVariable( cs(C,S) ):- course(C), slot(S).
satVariable( sr(S,R) ):- slot(S), room(R).
satVariable( ts(T,S) ):- slot(S),teacher(T).
satVariable( ys(Y,S) ):- year(Y), slot(S).
%%%%%%  2. Clause generation:

writeClauses:-
    oneSlotPerCourseLecture,
    atMostOneCourseLecturePerDay,
    oneRoomPerCourse,
    teacherMustBeInCourse,
    roomOnlyOneCourseAtSlot,
    creaCS,
    noOverlappingTeacher,
    respetaHorasProhibidas,
    creaTS,
    overlappingYear,
    creaYS,
    atMost6,
    % creaSR,
    true.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

creaYS:-
    year(Y),slot(S),
    findall(cls(C,L,S),(courseOfYear(Y,C),lectureOfCourse(C,L)),Lits),
    expressOr(ys(Y,S),Lits),
    fail.
creaYS.

atMost6:-
    year(Y),
    day(D),
    findall(ys(Y,S),slotOfDay(D,S),Lits),
    atMost(6,Lits),
    fail.
atMost6.

creaTS:-
    course(C),slot(S),teacher(T),lectureOfCourse(C,L),

    writeClause([-cls(C,L,S),-ct(C,T), ts(T,S)]),

    fail.
creaTS.

creaCS:-
    course(C),
    slot(S),
    findall(cls(C,L,S), lectureOfCourse(C,L), Res),
    expressOr(cs(C,S), Res),
    fail.
creaCS.

respetaHorasProhibidas:-
    teacher(T),horesProhibides(T,H),
    slot(S),
    findall(ts(T,S),member(S,H),Lits),
    exactly(0,Lits),
    fail.
respetaHorasProhibidas.

overlappingYear:-
    year(Y),
    assig(Y,C1,_,_,_),
    assig(Y,C2,_,_,_),
    slot(S),
    C1\=C2,

    atMost(1,[cs(C1,S),cs(C2,S)]),

    fail.
overlappingYear.

noOverlappingTeacher:-
    slot(S),teacher(T),
    assig(_,C1,_,_,T1),
    assig(_,C2,_,_,T2),
    C1\=C2,
    member(T,T1),
    member(T,T2),
    atMost(3,[cs(C1,S),cs(C2,S),ct(C1,T),ct(C2,T)]),

    fail.
noOverlappingTeacher.

teacherMustBeInCourse:-
    course(C),assig(_,C,_,_,LP),
    findall(ct(C,T),member(T,LP),Lits),
    exactly(1,Lits),
    fail.
teacherMustBeInCourse.

oneSlotPerCourseLecture:-
    course(C), lectureOfCourse(C,L),
    findall(cls(C,L,S),slot(S),Lits),
    exactly(1,Lits),
    fail.
oneSlotPerCourseLecture.

oneRoomPerCourse:-
    course(C),assig(_,C,_,L,_),
    findall(cr(C,R),member(R,L),Lits),
    exactly(1,Lits),
    fail.
oneRoomPerCourse.


roomOnlyOneCourseAtSlot:-
    room(R),
    slot(S),
    assig(_,C1,_,R1,_),
    assig(_,C2,_,R2,_),
    C1 \= C2,
    member(R,R2),
    member(R,R1),
    atMost(3, [cs(C1,S),cs(C2,S),cr(C1,R), cr(C2,R)]),
    fail.
roomOnlyOneCourseAtSlot.

atMostOneCourseLecturePerDay:-
    course(C),
    day(D),
    findall(cls(C,L,S),(slotOfDay(D,S),lectureOfCourse(C,L)),Lits),
    atMost(1,Lits),
    fail.
atMostOneCourseLecturePerDay.


%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:
displaySol(M):- slot(S),course(C),room(R), lectureOfCourse(C,L),teacher(T),courseOfYear(Y,C),
                member(cls(C,L,S), M),member(ct(C,T),M),member(cr(C,R),M),member(ts(T,S),M),escribeDia(S),
                write('hora '),write(S),
                write(' curso '),write(Y), 
                write(' assignatura ' ), write(C),
                write(' professor '),write(T), 
                write(' aula '),write(R),nl,
                fail.

% displaySol(M):- nl,teacher(T),slot(S),course(C),lectureOfCourse(C,L),
%                 member(ts(T,S),M),member(cls(C,L,S), M),member(ct(C,T),M),
%                 write(T), write(': '), write(S),nl,fail.

escribeDia(S)           :- S < 13, write('Lunes: ').
escribeDia(S)           :- S < 25,S>12, write('Martes: ').
escribeDia(S)           :- S < 37, S>24,write('Miercoles: ').
escribeDia(S)           :- S < 49,S>36, write('Jueves: ').
escribeDia(S)           :- S < 61,S>48, write('Viernes: ').









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !. 
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.
% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !. 
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeClause([ Var | NLits]), !.
%%%%%% Cardinality constraints on arbitrary sets of literals Lits:
exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.
atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
    negateAll(Lits,NLits),
    K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).
atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
    length(Lits,N),
    K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).
negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.
negate( -Var,  Var):-!.
negate(  Var, -Var):-!.
subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).
%%%%%% main:
main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
tell(header),  writeHeader,  told,
numVars(N), numClauses(C),
write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
shell('cat header clauses > infile.cnf',_),
write('Calling solver....'), nl,
shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result),!.
treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.
treatResult( _):- write('cnf input error. Wrote anything strange in your cnf?'), nl,nl, halt.
    
initClauseGeneration:-  %initialize all info about variables and clauses:
    retractall(numClauses(   _)),
    retractall(numVars(      _)),
    retractall(varNumber(_,_,_)),
    assert(numClauses( 0 )),
    assert(numVars(    0 )),     !.
writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!. 
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!. 
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.
% given the symbolic variable V, find its variable number N in the SAT solver:
:-dynamic(varNumber / 3).
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V
writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.
countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.
% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
