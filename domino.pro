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

bag_it([Last], Bag0) :- Bag0 = [Last]. %% Exhaust the pipe
%% Permutation the domino way
%% Stock is out-of-order
bag_it(Stock, Bag) :-
	select(First, Stock, Rest),
	bag_it(Rest, Bag0),
	nth1(1, Bag0, (X0, _)),
	last(Bag0,    (_, Y0)),

	(X1, Y1) = First,
	( Y0 == X1, append(Bag0, [(X1, Y1)], Bag)
	; X0 == X1, Bag = [(Y1, X1) | Bag0]
	; X0 == Y1, Bag = [(X1, Y1) | Bag0]
	; Y0 == Y1, append(Bag0, [(Y1, X1)], Bag)
	; fail
	).


bag(Ts, Bag) :-
	maplist(tile, Ts),
	maplist(identity_i, Ts, IDs),
	all_distinct(IDs),
	distinct(Identity,
		( bag_it(Ts, Bag)
		, line_identity(Bag, Identity)
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

bag_identity(Bag, BagIdentity) :-
	maplist(identity_i, Bag, IDs),
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
	findall([A, B, Bag], distinct(BagIdentity,
				( bag([(6,6), A, (2,1), B], Bag)
				, Bag = [(6,6), _, (2,1), _]
				, bag_identity([A, B], BagIdentity)
				))
			, Solutions),
	maplist(format('Solution: A: (~w) B: (~w) Line: ~w ~n'), Solutions).
