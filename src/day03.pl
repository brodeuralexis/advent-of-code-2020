:- module(day03, [day03/2]).

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lambda)).
:- use_module(library(lists)).

example([
    [open, open, tree, tree, open, open, open, open, open, open, open ],
    [tree, open, open, open, tree, open, open, open, tree, open, open ],
    [open, tree, open, open, open, open, tree, open, open, tree, open ],
    [open, open, tree, open, tree, open, open, open, tree, open, tree ],
    [open, tree, open, open, open, tree, tree, open, open, tree, open ],
    [open, open, tree, open, tree, tree, open, open, open, open, open ],
    [open, tree, open, tree, open, tree, open, open, open, open, tree ],
    [open, tree, open, open, open, open, open, open, open, open, tree ],
    [tree, open, tree, tree, open, open, open, tree, open, open, open ],
    [tree, open, open, open, tree, tree, open, open, open, open, tree ],
    [open, tree, open, open, tree, open, open, open, tree, open, tree ]
]).

entity(tree) --> "#".
entity(open) --> ".".

treeLine([]) --> eol.
treeLine([Entity | Entities]) --> entity(Entity), treeLine(Entities).

forest([]) --> [].
forest([TreeLine | TreeLines]) --> treeLine(TreeLine), forest(TreeLines).

forest_width([TreeLine | _], Width) :-
    length(TreeLine, Width).

forest_position_slope_next(Forest, X-Y, SlopeX-SlopeY, NextX-NextY) :-
    forest_width(Forest, Width),
    NextX #= (X + SlopeX) mod Width,
    NextY #= Y + SlopeY.

forest_position_entity(Forest, X-Y, Entity) :-
    nth0(Y, Forest, TreeLine),
    nth0(X, TreeLine, Entity).

forest_slope_trees(Forest, Slope, N) :-
    forest_slope_encounters(Forest, Slope, Encounters),
    include(=(tree), Encounters, Trees),
    length(Trees, N).

forest_slope_encounters(Forest, Slope, Encounters) :-
    forest_position_slope_encounters(Forest, 0-0, Slope, Encounters).

forest_position_slope_encounters(Forest, Position, Slope, []) :-
    forest_position_slope_next(Forest, Position, Slope, Next),
    \+ forest_position_entity(Forest, Next, _).
forest_position_slope_encounters(Forest, Position, Slope, [Entity | Encounters]) :-
    forest_position_slope_next(Forest, Position, Slope, Next),
    forest_position_entity(Forest, Next, Entity),
    forest_position_slope_encounters(Forest, Next, Slope, Encounters).

day03(Part1, Part2) :-
    phrase_from_file(forest(Forest), "./data/day03.txt"),
    part1(Forest, Part1),
    part2(Forest, Part2).

part1(Forest, Result) :-
    forest_slope_trees(Forest, 3-1, Result).

part2(Forest, Result) :-
    maplist(forest_slope_trees(Forest), [1-1, 3-1, 5-1, 7-1, 1-2], Trees),
    foldl(\X^Y^R^(R #= X * Y), Trees, 1, Result).
