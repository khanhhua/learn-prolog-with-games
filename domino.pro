%% DOMINO the game
:- use_module(library(clpfd)).

pip(0).
pip(1).
pip(2).
pip(3).
pip(4).
pip(5).
pip(6).

tile((X, Y)) :-
	pip(X),
	pip(Y).
	
%% tile has no direction
identity((X, Y), Identity) :-
	( X >= Y -> Identity is X * 10 + Y
	; Identity is Y * 10 + X
	).

equal((X, Y), (Y, X)).

connect(LastEnds, [], NewEnds) :- NewEnds = LastEnds.
connect(LastEnds, Ls, NewEnds) :-
	(X0, Y0) = LastEnds,
	[(X1, Y1) | Tail] = Ls,
	( Y0 == X1 -> NewLastEnds = (X0, Y1), connect(NewLastEnds, Tail, NewEnds)
	; X0 == X1 -> NewLastEnds = (Y0, Y1), connect(NewLastEnds, Tail, NewEnds)
	; X0 == Y1 -> NewLastEnds = (X1, Y0), connect(NewLastEnds, Tail, NewEnds)
	; Y0 == Y1 -> NewLastEnds = (X0, X1), connect(NewLastEnds, Tail, NewEnds)
	; fail
	).

line(Ts) :-
	maplist(tile, Ts),
	maplist(identity, Ts, IDs),
	all_distinct(IDs),

	[First | Tail] = Ts,
	( connect(First, Tail, _)
	; equal(First, AntiFirst) -> connect(AntiFirst, Tail, _)
	).

main :-
	write('Find out which tile fits in the following line:'),nl,
	write('[(6,6), (6,4), Which, (1,4)]'),nl,
	Tiles = [(6,6), (6,4), Which, (1,4)],
	findall(Which, distinct(Identity, (line(Tiles), identity(Which, Identity))), Solutions),
	format('Solutions: ~w ~n', [Solutions]).


