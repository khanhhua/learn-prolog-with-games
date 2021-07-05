%% Facts
rank(one).
rank(two).
rank(three).
rank(four).
rank(five).
rank(six).
rank(seven).
rank(eight).
rank(nine).
rank(ten).
rank(jack).
rank(queen).
rank(king).

suit(heart).
suit(diamond).
suit(club).
suit(spade).

%% What is a card
card((Rank, Suit)) :-
	suit(Suit),
	rank(Rank).

%% Rules for Cards
card_is_greater((RankX, SuitX), (RankY, SuitY)) :-
	card((RankX, SuitX)),
	card((RankY, SuitY)),
	( rank_is_greater(RankX, RankY)
	; rank_equal(RankX, RankY)
	, suit_is_greater(SuitX, SuitY)
	).

card_is_lesser((RankX, SuitX), (RankY, SuitY)) :-
	card_is_greater((RankY, SuitY), (RankX, SuitX)).

card_is_equal((RankX, SuitX), (RankX, SuitX)) :-
	card((RankX, SuitX)).


max_card(L, Out) :-
	max_card_search(L, [], Out).

max_card_search([], Temp, Out) :-
	nth0(0, Temp, Out).
max_card_search(L, [], Out) :-
	[ Head | Tail ] = L,
	max_card_search(Tail, [Head], Out).
max_card_search(L, Temp, Out) :-
	[ Head | Tail ] = L,
	length(Temp, 1),
	nth0(0, Temp, Candidate),
	( card_is_greater(Head, Candidate)
	-> max_card_search(Tail, [Head], Out)
	; max_card_search(Tail, Temp, Out)
	).

%% Rank numerical values
rank_value(one, 1).
rank_value(two, 2).
rank_value(three, 3).
rank_value(four, 4).
rank_value(five, 5).
rank_value(six, 6).
rank_value(seven, 7).
rank_value(eight, 8).
rank_value(nine, 9).
rank_value(ten, 0).
rank_value(jack, 10).
rank_value(queen, 10).
rank_value(king, 10).

rank_sum(Ranks, Sum) :-
	findall(Value, (rank_value(Rank, Value), member(Rank, Ranks)), Values),
	sum_list(Values, Sum).


%% Comparison
rank_greater(king, queen).
rank_greater(queen, jack).
rank_greater(jack, ten).
rank_greater(ten, nine).
rank_greater(nine, eight).
rank_greater(eight, seven).
rank_greater(seven, six).
rank_greater(six, five).
rank_greater(five, four).
rank_greater(four, three).
rank_greater(three, two).
rank_greater(two, one).


rank_is_greater(X0, Y0) :-
	rank_is_greater_cmp(X0, Y0, Out),
	X0 == Out.
rank_is_lesser(X0, Y0) :- rank_is_greater(Y0, X0).

rank_is_greater_cmp(X0, Y0, Out) :-
	( rank_greater(X0, Y0)
	, Out = X0
	; rank_greater(X, Y0)
	, rank_is_greater_cmp(X0, X, Out)
	).

rank_equal(X, X) :-
	rank(X).

rank_lesser(X, Y) :- rank_greater(Y,X).

suit_greater(heart, diamond).
suit_greater(diamond, club).
suit_greater(club, spade).
suit_is_greater(X0, Y0) :-
	suit_is_greater_cmp(X0, Y0, Out),
	X0 == Out.

suit_is_greater_cmp(X0, Y0, Out) :-
	( suit_greater(X0, Y0)
	, Out = X0
	; suit_greater(X, Y0)
	, suit_is_greater_cmp(X0, X, Out)
	).

suit_equal(X, X) :-
	suit(X).

suit_is_lesser(X0, Y0) :- suit_is_greater(Y0, X0).

%% Bacao
hand(((RankX,SuitX), (RankY,SuitY), (RankZ,SuitZ))) :-
	card((RankX,SuitX)),
	card((RankY,SuitY)),
	card((RankZ,SuitZ)).

hand_equal(Hand, Hand) :- hand(Hand).
	
hand_is_greater(HandX, HandT) :-
	hand(HandX),hand(HandT),
	not(hand_equal(HandX, HandT)),
	((RankX,SuitX), (RankY,SuitY), (RankZ,SuitZ)) = HandX,
	((RankT,SuitT), (RankU,SuitU), (RankV,SuitV)) = HandT,
	rank_sum([RankX, RankY, RankZ], SumX),
	rank_sum([RankT, RankU, RankV], SumT),
	( SumX > SumT
	; SumX == SumT
	, max_card([(RankX,SuitX), (RankY,SuitY), (RankZ,SuitZ)], MaxCardX)
	, max_card([(RankT,SuitT), (RankU,SuitU), (RankV,SuitV)], MaxCardT)
	, max_card([MaxCardX, MaxCardT], MaxCard)
	, MaxCard == MaxCardX
	).

:-	
	findall(C, card(C), AllCards),
	length(AllCards, TotalCards),
	format('Total cards in a deck: ~w ~n', [TotalCards]),

	%% What could the card be?
	format('Hand vs Hand:~n  ((one, spade), (two, heart), Card) > ((one, spade), (two, heart), (three, spade))~n'),
	findall(Card, hand_is_greater(
	  	((one, spade), (two, heart), Card),
	  	((one, spade), (two, heart), (king, spade))
 	), Solutions),
 	format('Solutions: ~w', [Solutions]).
