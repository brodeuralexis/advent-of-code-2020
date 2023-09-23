:- module(day07, [day07/2]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lambda)).

example([
    bag(`light red`, [`bright-white`=1, `muted yellow`=2]),
    bag(`dark orange`, [`bright white`=3, `muted yellow`=4]),
    bag(`bright white`, [`shiny gold`=1]),
    bag(`muted yellow`, [`shiny gold`=2, `faded blue`=9]),
    bag(`shiny gold`, [`dark olive`=1, `vibrant plum`=2]),
    bag(`dark olive`, [`faded blue`=3, `dotted black`=4]),
    bag(`vibrant plum`, [`faded blue`=5, `dotted black`=6]),
    bag(`faded blue`, []),
    bag(`dotted black`, [])
]).

bags([]) --> eos.
bags([bag(Name, Rules) | Bags]) --> string(Name), " bags contain ", rules(Rules), bags(Bags).

rules([]) --> ".", eol.
rules([]) --> "no other bags.", eol.
rules([Name=Quantity | Rules]) --> integer(Quantity), " ", string(Name), (" bag, " | " bags, " | " bag" | " bags"), rules(Rules).

rule_name_quantity(Name=Quantity, Name, Quantity).

bags_name_contain(Bags, Name, Contain) :-
    member(bag(Name, Rules), Bags),
    member(Contain=_, Rules).
bags_name_contain(Bags, Name, Contain) :-
    member(bag(Name, Rules), Bags),
    member(SubName=_, Rules),
    bags_name_contain(Bags, SubName, Contain).

bags_name_count(Bags, Name, Count) :-
    member(bag(Name, Rules), Bags),
    maplist(rule_name_quantity, Rules, Names, Quantities),
    maplist(bags_name_count(Bags), Names, Counts),
    maplist(\X^Y^P^(P #= X * (1 + Y)), Quantities, Counts, Counts0),
    sum_list(Counts0, Count).

day07(Part1, Part2) :-
    phrase_from_file(bags(Bags), "./data/day07.txt"),
    part1(Bags, Part1),
    part2(Bags, Part2).

part1(Bags, Result) :-
    findall(Name, member(bag(Name, _), Bags), Names),
    include(\Name^bags_name_contain(Bags, Name, `shiny gold`), Names, Carryables),
    length(Carryables, Result).

part2(Bags, Result) :-
    bags_name_count(Bags, `shiny gold`, Result).
