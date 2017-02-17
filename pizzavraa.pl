% Constraint Logic Programming
:- use_module(library(dif)).		% Sound inequality
:- use_module(library(clpfd)).		% Finite domain constraints
:- use_module(library(clpb)).		% Boolean constraints
:- use_module(library(chr)).		% Constraint Handling Rules
:- use_module(library(when)).		% Coroutining

small(6, 7, 1, 5, [[t, m, m, m, t, t, t],
[m, m, m, m, t, m, m],
[t, t, m, t, t, m, t],
[t, m, m, t, m, m, m],
[t, t, t, t, t, t, m],
[t, t, t, t, t, t, m]]).

row(M, N, Row) :-
    nth1(N, M, Row).

first_column([], [], []).
first_column([[]|_], [], []).
first_column([[I|Is]|Rs], [I|Col], [Is|Rest]) :-
    first_column(Rs, Col, Rest).

transp([[]|_], []) :- !.
transp([[I|Is]|Rs], [Col|MT]) :-
    first_column([[I|Is]|Rs], Col, [Is|NRs]),
    transp([Is|NRs], MT).

column(M, N, Col) :-
    transp(M, MT),
    row(MT, N, Col).

at(Row, Col, I):-
    input(_, _, _, _, Pizza), 
    row(Pizza, Row, IPartial),
    column([IPartial], Col, [I]).

count([], LP, LP):-!.
count([H|T], LP, LF):- atom(H), !, LP2 is LP + 1, count(T, LP2, LF).
count([H|T], LP, LF):-count(H, 0, LFR), LP2 is LP + LFR, count(T, LP2, LF).

countM([], LP, LP):-!.
countM([H|T], LP, LF):- atom(H), H == m, !, LP2 is LP + 1, countM(T, LP2, LF).
countM([H|T], LP, LF):- atom(H), H == t, !, countM(T, LP, LF).
countM([H|T], LP, LF):-countM(H, 0, LFR), LP2 is LP + LFR, countM(T, LP2, LF).    

countT([], LP, LP):-!.
countT([H|T], LP, LF):- atom(H), H == t, !, LP2 is LP + 1, countT(T, LP2, LF).
countT([H|T], LP, LF):- atom(H), H == m, !, countT(T, LP, LF).
countT([H|T], LP, LF):-countT(H, 0, LFR), LP2 is LP + LFR, countT(T, LP2, LF).  

:-dynamic(slices/1).
slices(0).
:-dynamic(solution/1).
solution([]).


nonoverlap([R11, R21, C11, C21], [R12, R22, C12, C22]):- C21 < C12, !.
nonoverlap([R11, R21, C11, C21], [R12, R22, C12, C22]):- C11 > C22, !.
nonoverlap([R11, R21, C11, C21], [R12, R22, C12, C22]):- R21 < R12, !.
nonoverlap([R11, R21, C11, C21], [R12, R22, C12, C22]):- R11 > R22, !.
nonoverlap([], _).

coherent([H|T], NewSol):-nonoverlap(H, NewSol), coherent(T, NewSol).
coherent([], _).

getPizzaRows(R1, R2, [Row|PR], Pizza):-
    R1 =< R2, !,
    row(Pizza, R1, Row),
    R1P is R1 + 1,
    getPizzaRows(R1P, R2, PR, Pizza).
getPizzaRows(_, _, [], _).

getPizzaCols(C1, C2, [Col|PC], PizzaRows):-
    C1 =< C2, !,
    column(PizzaRows, C1, Col),
    C1P is C1 + 1,
    getPizzaCols(C1P, C2, PC, PizzaRows).
getPizzaCols(_, _, [], _).

getPizza(R1, R2, C1, C2, Pizza, PizzaR):-
    getPizzaRows(R1, R2, PR, Pizza),
    getPizzaCols(C1, C2, PizzaR, PR).

betw(I,J,I) :- I =< J.
betw(I,J,K) :- I < J, I1 is I+1, betw(I1,J,K).

solve:- input(R, C, L, H, Pizza),
    betw(1, R, R1),
    betw(1, R, R2),
    betw(1, C, C1),
    betw(1, C, C2),
    R1 =< R2,
    C1 =< C2,
  	partialSolution(Sol),
    coherent(Sol, [R1, R2, C1, C2]),
    getPizza(R1, R2, C1, C2, Pizza, PizzaR),
    count(PizzaR, 0, HPR),
    HPR =< H,
    countM(PizzaR, 0, MPR),
    countT(PizzaR, 0, TPR),
    MIN is min(MPR, TPR),
    MIN >= L,
    assert(solution([R1, R2, C1, C2])),
    retract(slices(S)),
    SP is S + 1,
    assert(slices(SP)),
    fail.
solve.


partialSolution([H|T]):-
    retract(solution(H)), !,
    partialSolution(T),
    assert(solution(H)).
partialSolution([]).

writeReallyCool([H|T]):-write(H), write(' '), writeReallyCool(T).
writeReallyCool([]).

list_empty([]).

writeCool([H|T]):-list_empty(H), !, writeCool(T).
writeCool([H|T]):-writeReallyCool(H), nl, writeCool(T).
writeCool([]).

pizza:-
    consult("io.pl"),
    readInputFile,
    solve,
    retract(slices(SliceCount)),
    partialSolution(Solution),
    writeOutputFile(SliceCount, Solution).
