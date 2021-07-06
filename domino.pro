%% DOMINO the game
unit(1).
unit(2).
unit(3).
unit(4).
unit(5).
unit(6).

tile(X, Y) :-
	unit(X),
	unit(Y).

%% tile has no direction
identical(tile(Y, X), Identical) :-
	Identical = tile(X, Y).