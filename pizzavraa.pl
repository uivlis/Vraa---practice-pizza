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

transpose([[]|_], []) :- !.
transpose([[I|Is]|Rs], [Col|MT]) :-
    first_column([[I|Is]|Rs], Col, [Is|NRs]),
    transpose([Is|NRs], MT).

column(M, N, Col) :-
    transpose(M, MT),
    row(MT, N, Col).

at(Row, Col, I):-
    small(_, _, _, _, Pizza), 
    row(Pizza, Row, IPartial),
    column([IPartial], Col, [I]).

solve(Slices, Solution):- small(R, C, L, H, Pizza),
    at(2, 5, Solution),
    Slices #= 1.
