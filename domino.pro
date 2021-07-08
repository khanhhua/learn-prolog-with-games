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
	
%% Identity without direction, as a tile is not put in a line
identity_i((X, Y), Identity) :-
	( X >= Y -> Identity is X * 10 + Y + 1
	; Identity is Y * 10 + X + 1
	).

%% Identity with direction, it is put in a line, its direction matters
identity((X, Y), Identity) :- Identity is X * 10 + Y + 1.

equal((X, Y), (Y, X)).

line_it([Last], Line0) :- Line0 = [Last]. %% Exhaust the pipe
%% Permutation the domino way
%% Stock is out-of-order
line_it(Stock, Line) :-
	select(First, Stock, Rest),
	line_it(Rest, Line0),
	nth1(1, Line0, (X0, _)),
	last(Line0,    (_, Y0)),

	(X1, Y1) = First,
	( Y0 == X1, append(Line0, [(X1, Y1)], Line)
	; X0 == X1, Line = [(Y1, X1) | Line0]
	; X0 == Y1, Line = [(X1, Y1) | Line0]
	; Y0 == Y1, append(Line0, [(Y1, X1)], Line)
	; fail
	).


line(Ts, Line) :-
	maplist(tile, Ts),
	maplist(identity_i, Ts, IDs),
	all_distinct(IDs),
	distinct(Identity,
		( line_it(Ts, Line)
		, line_identity(Line, Identity)
		)
	).
	

line_identity(Line, LineIdentity) :-
	maplist(identity, Line, IDs),
	findall(Identity * Scale, %% Projection aka Template
		( member(Identity, IDs)
		, nth0(Index, IDs, Identity)
		, pow(12, Index, Scale)
		) %% Composite Goal, aka Predicate
		, CorrelatedIDs
	),
	sum_list(CorrelatedIDs, LineIdentity).

bag_identity(Line, BagIdentity) :-
	maplist(identity_i, Line, IDs),
	sort(IDs, SortedIDs),
	findall(Identity * Scale, %% Projection aka Template
		( member(Identity, SortedIDs)
		, nth0(Index, SortedIDs, Identity)
		, pow(100, Index, Scale)
		) %% Composite Goal, aka Predicate
		, CorrelatedIDs
	),
	sum_list(CorrelatedIDs, BagIdentity).

main :-
	write('Find out how to form a line with (6,6), A, (2,1), B:'),nl,
	findall([A, B, Line], distinct(LineIdentity,
				( line([(6,6), A, (2,1), B], Line)
				, Line = [(6,6), A, (2,1), B]
				, line_identity([A, B], LineIdentity)
				))
			, Solutions),
	maplist(format('Solution: A: (~w) B: (~w) Line: ~w ~n'), Solutions).
