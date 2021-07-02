:- dynamic shoot_what/3.

%% Facts
hand(rock).
hand(paper).
hand(scissors).

%% Relationships
wins(rock, scissors).
wins(paper, rock).
wins(scissors, paper).

draws(X, X) :- true.
loses(X, Y) :- wins(Y, X), not(draws(X, Y)).

%% Scenario
%% Three kids playing Rock-Paper-Scissors: Mary, John and Jane.
%% For a given shoot by Mary and Jane, John wanna win/draw Jane and lose to Mary.
%% Is there such a solution?
shoot_what(Mary, Jane, John) :-
	wins(Mary, John),
	(loses(Jane, John); draws(Jane, John)).

countS(M, J, Acc0, Acc) :-
	findall(John, shoot_what(M, J, John), S),
	append(Acc0, S, Acc).

:-
	%% START OF SCENARIO INPUT
	Mary = [ rock, rock, paper, rock, scissors, paper ],
	Jane = [ scissors, paper, scissors, paper, rock, scissors ],
	%% END OF SCENARIO INPUT
	foldl(countS, Mary, Jane, [], Solutions),
	( Solutions == []
	-> write('No solution') 
	; format('John: ~w', [Solutions])
	).