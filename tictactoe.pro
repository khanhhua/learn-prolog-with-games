% Cell facts - for later user input validation
cell(r0,c0).
cell(r0,c1).
cell(r0,c2).
cell(r1,c0).
cell(r1,c1).
cell(r1,c2).
cell(r2,c0).
cell(r2,c1).
cell(r2,c2).

% Winning conditions
line((X,c0), (X,c1), (X,c2)).
line((r0,Y), (r1,Y), (r2,Y)).
line((r0, c0), (r1, c1), (r2,c2)).
line((r2, c0), (r1, c1), (r0,c2)).

% Initial condition
% +--+--+--+
% |P1|  |  |
% |--+--+--|
% |  |P1|  |
% +--+--+--+
% |  |  |P1|
% +--+--+--+
place(p1, (r1,c1)).
place(p2, (r1,c0)).
place(p1, (r2,c2)).
place(p2, (r2,c0)).
place(p1, (r0,c0)).

% Query for the winner
who_wins(P) :-
    setof(C, place(P, C), Cells),
    sort(Cells, Sorted),
    length(Sorted, 3),
    [C0, C1, C2] = Sorted,
    line(C0, C1, C2).

declare_winner([]) :- writeln('No winner').
declare_winner(Winner) :- format('Winner: ~w', Winner).

% Main
:-
    setof(P, who_wins(P), Winner),
    declare_winner(Winner).