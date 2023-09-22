:- module(day04, [day04/2]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

batch([[]]) --> eos.
batch([[] | Data]) --> "\n", batch(Data).

batch(NewData) -->
    string(Field),
    ":",
    string(Value),
    ("\n" | " "),
    batch(Data),
    {
        Data = [X|OldData],
        atom_string(AField, Field),
        Y =.. [AField, Value],
        append(X, [Y], NewX),
        NewData = [NewX|OldData]
    }.

example([
    [ ecl(`gry`), pid(`860033327`), eyr(`2020`), hcl(`#fffffd`)
    , byr(`1937`), iyr(`2017`), cid(`147`), hgt(`183cm`)],
    [ iyr(`2013`), ecl(`amb`), cid(`350`), eyr(`2023`), pid(`028048884`)
    , hcl(`#cfa07d`), byr(`1929`) ],
    [ hcl(`#ae17e1`), iyr(`2013`)
    , eyr(`2024`)
    , ecl(`brn`), pid(`760753108`), byr(`1931`)
    , hgt(`179cm`) ],
    [ hcl(`#cfa07d`), eyr(`2025`), pid(`166559648`)
    , iyr(`2011`), ecl(`brn`), hgt(`59in`) ]
]).

eyeColor(amb).
eyeColor(blu).
eyeColor(brn).
eyeColor(gry).
eyeColor(grn).
eyeColor(hzl).
eyeColor(oth).

validPassport(Passport) :-
    member(byr(_), Passport),
    member(iyr(_), Passport),
    member(eyr(_), Passport),
    member(hgt(_), Passport),
    member(hcl(_), Passport),
    member(ecl(_), Passport),
    member(pid(_), Passport).

height --> integer(Height), "cm", { Height #>= 150, Height #=< 193 }.
height --> integer(Height), "in", { Height #>= 59, Height #=< 76 }.

validPassportData(Passport) :-
    member(byr(BirthYear), Passport),
    year_range(BirthYear, 1920-2002),
    member(iyr(IssueYear), Passport),
    year_range(IssueYear, 2010-2020),
    member(eyr(ExpirationYear), Passport),
    year_range(ExpirationYear, 2020-2030),
    member(hgt(Height), Passport),
    phrase(height, Height),
    member(hcl(HairColor), Passport),
    phrase(("#", xdigits(_)), HairColor),
    member(ecl(EyeColor0), Passport),
    atom_codes(EyeColor, EyeColor0),
    eyeColor(EyeColor),
    member(pid(PassportID), Passport),
    phrase(digits(Digits), PassportID),
    length(Digits, 9).

year_range(Year0, Range) :-
    phrase(integer(Year), Year0),
    year_range(Year, Range).
year_range(Year, Start-End) :-
    integer(Year),
    Year #>= Start, Year #=< End.

day04(Part1, Part2) :-
    phrase_from_file(batch(Passports), "./data/day04.txt"),
    part1(Passports, Part1),
    part2(Passports, Part2).

part1(Passports, Result) :-
    include(validPassport, Passports, ValidPassports),
    length(ValidPassports, Result).

part2(Passports, Result) :-
    include(validPassportData, Passports, ValidPassports),
    length(ValidPassports, Result).
