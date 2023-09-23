:- module(day08, [day08/2]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(pio)).

argument(Argument) --> "+", integer(Argument).
argument(Argument) --> "-", integer(Argument0), { Argument #= -Argument0 }.

instruction(nop(Argument)) --> "nop ", argument(Argument).
instruction(jmp(Argument)) --> "jmp ", argument(Argument).
instruction(acc(Argument)) --> "acc ", argument(Argument).

instructions([]) --> eos.
instructions([Instruction | Instructions]) --> instruction(Instruction), eol, instructions(Instructions).

instruction_state0_state(nop(_), state(IP0, Acc0), state(IP, Acc0)) :-
    IP #= IP0 + 1.
instruction_state0_state(acc(Argument), state(IP0, Acc0), state(IP, Acc)) :-
    IP #= IP0 + 1,
    Acc #= Acc0 + Argument.
instruction_state0_state(jmp(Argument), state(IP0, Acc0), state(IP, Acc0)) :-
    IP #= IP0 + Argument.

instructions_state0_state(Instructions, State0, State) :-
    instructions_visited_state0_state(Instructions, [], State0, State).

instructions_visited_state0_state(Instructions, _, state(IP0, Acc0), state(done, Acc0)) :-
    \+ nth0(IP0, Instructions, _).
instructions_visited_state0_state(_, Visited, state(IP0, Acc0), state(IP0, Acc0)) :-
    member(IP0, Visited).
instructions_visited_state0_state(Instructions, Visited0, state(IP0, Acc0), state(IP, Acc)) :-
    \+ member(IP0, Visited0),
    nth0(IP0, Instructions, Instruction),
    instruction_state0_state(Instruction, state(IP0, Acc0), state(IP1, Acc1)),
    Visited = [IP0 | Visited0],
    instructions_visited_state0_state(Instructions, Visited, state(IP1, Acc1), state(IP, Acc)).

day08(Part1, Part2) :-
    phrase_from_file(instructions(Instructions), "./data/day08.txt"),
    part1(Instructions, Part1),
    part2(Instructions, Part2).

part1(Instructions, Result) :-
    instructions_state0_state(Instructions, state(0, 0), state(_, Result)).

originalInstruction_exchangedInstruction(nop(Argument), jmp(Argument)) :- Argument #\= 0.
originalInstruction_exchangedInstruction(jmp(Argument), nop(Argument)).

part2(Instructions, Result) :-
    length(Instructions, N),
    between(0, N, L),
    nth0(L, Instructions, Original, R),
    nth0(L, ExchangedInstructions, Exchanged, R),
    originalInstruction_exchangedInstruction(Original, Exchanged),
    instructions_state0_state(ExchangedInstructions, state(0, 0), state(done, Result)).
