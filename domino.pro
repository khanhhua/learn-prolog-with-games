%% DOMINO the game
:- use_module(library(clpfd)).

pip(X) :-
	X #>= 0,
	X #=< 6.

%% tile has no direction
tile((X, Y)) :-
	pip(X),
	pip(Y).


identity((X, Y), Identity) :-
	( X #>= Y -> Identity #= X * 10 + Y
	; Identity #= Y * 10 + X
	).

equal((X, Y), (Y, X)).

connect(LastEnds, [], NewEnds) :- NewEnds = LastEnds.
connect(LastEnds, Ls, NewEnds) :-
	(X0, Y0) = LastEnds,
	[(X1, Y1) | Tail] = Ls,
	( Y0 #= X1 -> NewLastEnds = (X0, Y1), connect(NewLastEnds, Tail, NewEnds)
	; X0 #= X1 -> NewLastEnds = (Y0, Y1), connect(NewLastEnds, Tail, NewEnds)
	; X0 #= Y1 -> NewLastEnds = (X1, Y0), connect(NewLastEnds, Tail, NewEnds)
	; Y0 #= Y1 -> NewLastEnds = (X0, X1), connect(NewLastEnds, Tail, NewEnds)
	; fail
	).

line(A, B, C, NewEnds) :-
	maplist(tile, [A,B,C]),
	maplist(identity, [A,B,C], IDs),
	all_distinct(IDs),
	
	( connect(A, [B, C], NewEnds)
	; equal(A, AntiA) -> connect(AntiA, [B, C], NewEnds)
	).