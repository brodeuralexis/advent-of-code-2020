:- module(day02, [day02/2]).

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lambda)).
:- use_module(library(lists)).

example([
    row(policy(1-3, 97), `abcde`),
    row(policy(1-3, 98), `cdefg`),
    row(policy(2-9, 99), `ccccccccc`)
]).

range(Start-End) --> integer(Start), "-", integer(End).
password(Password) --> string_without("\n", Password).

policy(policy(Range, Letter)) --> range(Range), " ", [Letter].

database([]) --> eos.
database([row(Policy, Password) | Rows]) --> policy(Policy), ": ", password(Password), eol, database(Rows).

oldValidRow(row(Policy, Password)) :-
    oldPolicy_password(Policy, Password).

oldPolicy_password(policy(Start-End, Letter), Password) :-
    include(#=(Letter), Password, Letters),
    length(Letters, N),
    N #>= Start, N #=< End.

newValidRow(row(Policy, Password)) :-
    newPolicy_password(Policy, Password).

newPolicy_password(policy(First-Second, Letter), Password) :-
    nth1(First, Password, Letter),
    \+ nth1(Second, Password, Letter).
newPolicy_password(policy(First-Second, Letter), Password) :-
    \+ nth1(First, Password, Letter),
    nth1(Second, Password, Letter).

day02(Part1, Part2) :-
    phrase_from_file(database(Rows), "./data/day02.txt"),
    part1(Rows, Part1),
    part2(Rows, Part2).

part1(Rows, Result) :-
    include(oldValidRow, Rows, ValidRows),
    length(ValidRows, Result).

part2(Rows, Result) :-
    include(newValidRow, Rows, ValidRows),
    length(ValidRows, Result).
