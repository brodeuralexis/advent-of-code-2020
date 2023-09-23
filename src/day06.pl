:- module(day06, [day06/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(lambda)).
:- use_module(library(pio)).

groups([[]]) --> eos.
groups([[] | Groups]) --> "\n", groups(Groups).

groups(Groups) -->
    string(Answers),
    "\n",
    groups(Data),
    {
        Data = [Group | OldGroups],
        append(Group, [Answers], NewGroup),
        Groups = [NewGroup | OldGroups]
    }.

day06(Part1, Part2) :-
    phrase_from_file(groups(Groups), "data/day06.txt"),
    part1(Groups, Part1),
    part2(Groups, Part2).

part1(Groups, Result) :-
    maplist(\L^V^foldl(union, L, [], V), Groups, Answers),
    maplist(length, Answers, Counts),
    sum_list(Counts, Result).

part2(Groups, Result) :-
    All = `abcdefghijklmnopqrstuvwxyz`,
    maplist(\L^V^foldl(intersection, L, All, V), Groups, Answers),
    maplist(length, Answers, Counts),
    sum_list(Counts, Result).
