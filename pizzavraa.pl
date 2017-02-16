% Constraint Logic Programming
:- use_module(library(dif)).		% Sound inequality
:- use_module(library(clpfd)).		% Finite domain constraints
:- use_module(library(clpb)).		% Boolean constraints
:- use_module(library(chr)).		% Constraint Handling Rules
:- use_module(library(when)).		% Coroutining

small(6, 7, 1, 5, [[T, M, M, M, T, T, T],
[M, M, M, M, T, M, M],
[T, T, M, T, T, M, T],
[T, M, M, T, M, M, M],
[T, T, T, T, T, T, M],
[T, T, T, T, T, T, M]]).

solve(Slices, Solution):- small(R, C, L, H, Pizza)
    %tbc
    .
