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
    small(_, _, _, _, Pizza), 
    row(Pizza, Row, IPartial),
    column([IPartial], Col, [I]).

count([], LP, LP):-!.
count([H|T], LP, LF):- atom(H), !, LP2 #= LP + 1, count(T, LP2, LF).
count([H|T], LP, LF):-count(H, 0, LFR), LP2 #= LP + LFR, count(T, LP2, LF).

countM([], LP, LP):-!.
countM([H|T], LP, LF):- atom(H), H #= m, !, LP2 #= LP + 1, countM(T, LP2, LF).
countM([H|T], LP, LF):-countM(H, 0, LFR), LP2 #= LP + LFR, countM(T, LP2, LF).    

countT([], LP, LP):-!.
countT([H|T], LP, LF):- atom(H), H #= t, !, LP2 #= LP + 1, countT(T, LP2, LF).
countT([H|T], LP, LF):-countT(H, 0, LFR), LP2 #= LP + LFR, countT(T, LP2, LF).  

:-dynamic(slices/1).
slices(0).
:-dynamic(solution/1).
solution([]).

nonoverlap([R11, R21, C11, C21], [R12, R22, C12, C22]):- R21 #=< R22, C21 #=< C22.

coherent([H|T], NewSol):-nonoverlap(H, NewSol), coherent(T, NewSol).
coherent([], _).

getPizzaRows(R1, R2, [Row|PR], Pizza):-
    R1 #=< R2,
    R1P #= R1 + 1,
    row(Pizza, R1P, Row),
    getPizzaRows(R1P, R2, PR).
getPizzaRows(_, _, []).

getPizzaCols(C1, C2, [Col|PC], PizzaRows):-
    C1 #=< C2,
    C1P #= C1 + 1,
    row(PizzaRows, C1P, Col),
    getPizzaCols(C1P, C2, PC).
getPizzaCols(_, _, []).

getPizza(R1, R2, C1, C2, Pizza, PizzaR):-
    getPizzaRows(R1, R2, PR, Pizza),
    getPizzaCols(C1, C2, PizzaR, PR).

solve:- small(R, C, L, H, Pizza),
    RR #= R - 1,
    CC #= C - 1,
    [R1, R2] ins 0..RR,
    [C1, C2] ins 0..CC,
    R1 #=< R2,
    C1 #=< C2,
    retract(solution(Sol)),
    coherent(Sol, [R1, R2, C1, C2]),
    getPizza(R1, R2, C1, C2, Pizza, PizzaR),
    count(PizzaR, 0, HPR),
    HPR #=< H,
    countM(PizzaR, 0, MPR),
    countT(PizzaR, 0, TPR),
    min(MPR, TPR) #>= L,
    assert(solution([[R1, R2, C1, C2]|Sol])),
    retract(slices(S)),
    SP #= S + 1,
    assert(slices(SP)),
    fail.
solve.

pizza:-solve,
    retract(slices(S)),
    write(S), nl,
    retract(solution(Sol)),
    write(Sol).
