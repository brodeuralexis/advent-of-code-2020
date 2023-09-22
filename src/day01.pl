:- module(day01, [day01/2]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(pio)).

expense(E) --> integer(E).

expenses([]) --> eos.
expenses([E | Es]) --> expense(E), eol, expenses(Es).

day01(Part1, Part2) :-
    phrase_from_file(expenses(Expenses), "./data/day01.txt"),
    part1(Expenses, Part1),
    part2(Expenses, Part2).

expenses_sum_entries(Expenses, Sum, Entries) :-
    is_set(Entries),
    maplist(\E^(member(E, Expenses)), Entries),
    sum_list(Entries, Sum).

part1(Expenses, Result) :-
    expenses_sum_entries(Expenses, 2020, [N1, N2]),
    Result #= N1 * N2.

part2(Expenses, Result) :-
    expenses_sum_entries(Expenses, 2020, [N1, N2, N3]),
    Result #= N1 * N2 * N3.
