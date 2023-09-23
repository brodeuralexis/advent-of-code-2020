:- module(day05, [day05/2]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(pio)).

example([
    pass([1, 0, 0, 0, 1, 1, 0], [1, 1, 1]),
    pass([0, 0, 0, 1, 1, 1, 0], [1, 1, 1]),
    pass([1, 1, 0, 0, 1, 1, 0], [1, 0, 0])
]).

column(0) --> "L".
column(1) --> "R".
row(0) --> "F".
row(1) --> "B".

rows([R1, R2, R3, R4, R5, R6, R7]) -->
    row(R1),
    row(R2),
    row(R3),
    row(R4),
    row(R5),
    row(R6),
    row(R7).

columns([C1, C2, C3]) -->
    column(C1),
    column(C2),
    column(C3).

pass(pass(Rows, Columns)) --> rows(Rows), columns(Columns).

passes([]) --> eos.
passes([Pass | Passes]) --> pass(Pass), eol, passes(Passes).

binary_integer([], 0).
binary_integer([Bit | Bits], Integer) :-
    length(Bits, Length),
    Base is Bit << Length,
    binary_integer(Bits, Integer0),
    Integer #= Base + Integer0.

pass_seat(pass(Rows, Columns), Row-Column) :-
    binary_integer(Rows, Row),
    binary_integer(Columns, Column).

seat_id(Row-Column, Id) :-
    Id #= Row * 8 + Column.

day05(Part1, Part2) :-
    phrase_from_file(passes(Passes), "./data/day05.txt"),
    part1(Passes, Part1),
    part2(Passes, Part2).

part1(Passes, Result) :-
    maplist(pass_seat, Passes, Seats),
    maplist(seat_id, Seats, Ids),
    max_list(Ids, Result).

part2(Passes, Result) :-
    maplist(pass_seat, Passes, Seats),
    findall(MissingSeat, (
        between(20, 107, Row),
        between(0, 7, Column),
        MissingSeat = Row-Column,
        \+ member(MissingSeat, Seats)
    ), [Seat | _]),
    seat_id(Seat, Result).
