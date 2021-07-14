:- use_module(library(clpfd)).
:- dynamic optimize/2.
:- dynamic concurrent_optimize/2.

%% Facts
rank(ace).
rank(pig).
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


card((R, S)) :-
	rank(R),
	suit(S).

next_rank(three, four).
next_rank(four, five).
next_rank(five, six).
next_rank(six, seven).
next_rank(seven, eight).
next_rank(eight, nine).
next_rank(nine, ten).
next_rank(ten, jack).
next_rank(jack, queen).
next_rank(queen, king).
next_rank(king, ace).
next_rank(ace, pig).

value(three, 1).
value(four,  2).
value(five,  3).
value(six,   4).
value(seven, 5).
value(eight, 6).
value(nine,  7).
value(ten,   8).
value(jack,  9).
value(queen,10).
value(king, 11).
value(ace,  12).
value(pig,  13).

value(heart, 400).
value(diamond, 300).
value(club, 200).
value(spade, 100).

value((R,S), Value) :-
	card((R,S)),
	value(R, RValue),
	value(S, SValue),
	Value is SValue + RValue.

value(b(A, B, C, D), Value) :-
	maplist(value, [A, B, C, D], CardValues),
	sum_list(CardValues, Sum),
	Value is Sum * 100000.
value(c(A, B, C, D, E, F), Value) :-
	maplist(value, [A, B, C, D, E, F], CardValues),
	sum_list(CardValues, Sum),
	Value is Sum * 10000.
value(st(Straight), Value) :-
	maplist(value, Straight, CardValues),
	length(Straight, Length),
	last(Straight, Last),
	sum_list(CardValues, Sum),
	value(Last, ValueLead),
	Value is Sum * Length * 1000 + ValueLead.
value(t(A, B, C), Value) :-
	maplist(value, [A, B, C], CardValues),
	sum_list(CardValues, Sum),
	Value is Sum * 100.
value(p(A, B), Value) :-
	maplist(value, [A, B], CardValues),
	sum_list(CardValues, Sum),
	Value is Sum * 10.
value(s(A), Value) :- value(A, Value).
value(Groups, Value) :-
	maplist(value, Groups, GroupValues),
	sum_list(GroupValues, Value).

/* Priorities */
heavier(b(A1,A2,A3,A4), b(B1,B2,B3,B4), HeavyGroup) :-
	value(b(A1,A2,A3,A4), Avalue),
	value(b(B1,B2,B3,B4), Bvalue),
	( Avalue > Bvalue -> HeavyGroup = b(A1,A2,A3,A4)
	; HeavyGroup = b(B1,B2,B3,B4)
	).
heavier(b(A1,A2,A3,A4), _, b(A1,A2,A3,A4)).

heavier(c(A1,A2,A3,A4,A5,A6), c(B1,B2,B3,B4,B5,B6), HeavyGroup) :-
	value(c(A1,A2,A3,A4,A5,A6), Avalue),
	value(c(B1,B2,B3,B4,B5,B6), Bvalue),
	( Avalue > Bvalue -> HeavyGroup = c(A1,A2,A3,A4,A5,A6)
	; HeavyGroup = c(B1,B2,B3,B4,B5,B6)
	).
heavier(c(A1,A2,A3,A4,A5,A6), _, c(A1,A2,A3,A4,A5,A6)).

heavier(t(A1,A2,A3), t(B1,B2,B3), HeavyGroup) :-
	value(t(A1,A2,A3), Avalue),
	value(t(B1,B2,B3), Bvalue),
	( Avalue > Bvalue -> HeavyGroup = t(A1,A2,A3)
	; HeavyGroup = t(B1,B2,B3)
	).
heavier(t(A1,A2,A3), _, t(A1,A2,A3)).

heavier(p(A1,A2), p(B1,B2), HeavyGroup) :-
	value(p(A1,A2), Avalue),
	value(p(B1,B2), Bvalue),
	( Avalue > Bvalue -> HeavyGroup = p(A1,A2)
	; HeavyGroup = p(B1,B2)
	).
heavier(p(A1,A2), _, p(A1,A2)).

heavier(s(A1), s(B1), HeavyGroup) :-
	value(s(A1), Avalue),
	value(s(B1), Bvalue),
	( Avalue > Bvalue -> HeavyGroup = s(A1)
	; HeavyGroup = s(B1)
	).


%% DCG Grammar for Hand Structure

is_bomb(b(A, B, C, D))  -->
	[A], [B], [C], [D],
	{ card((R, S1)) = card(A)
	, card((R, S2)) = card(B)
	, card((R, S3)) = card(C)
	, card((R, S4)) = card(D)
	, distinct(Member, member(Member, [S1, S2, S3, S4]))
	}.

is_chop(c(A, B, C, D, E, F)) -->
	[A], [B], [C], [D], [E], [F],
	{ card((R1, _)) = card(A), next_rank(R1, R2), next_rank(R2, R3)
	, card((R1, _)) = card(B)
	, card((R2, _)) = card(C)
	, card((R2, _)) = card(D)
	, card((R3, _)) = card(E)
	, card((R3, _)) = card(F)
	}.

is_triple(t(A,B,C)) -->
	[A], [B], [C],
	{ card((R1, _)) = card(A)
	, card((R1, _)) = card(B)
	, card((R1, _)) = card(C)
	}.

is_double(p(A,B)) -->
	[A], [B],
	{ card((R1, _)) = card(A)
	, card((R1, _)) = card(B)
	}.

is_single(s(A)) --> [A], {card(A)}.

%% 3-4-5-6-7-8-9-10-J-Q-K-A
%% A-B-C-D-E-F-G-H-I-J-K-L
is_straight(st(As)) -->
	[A], [B], [C], [D], [E], [F], [G], [H], [I], [J], [K], [L],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4), next_rank(R4, R5)
	, next_rank(R5, R6), next_rank(R6, R7), next_rank(R7, R8), next_rank(R8, R9)
	, next_rank(R9, R10), next_rank(R10, R11), next_rank(R11, R12)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, card((R5, _)) = card(E)
	, card((R6, _)) = card(F)
	, card((R7, _)) = card(G)
	, card((R8, _)) = card(H)
	, card((R9, _)) = card(I)
	, card((R10, _)) = card(J)
	, card((R11, _)) = card(K)
	, card((R12, _)) = card(L)
	, As = [A,B,C,D,E,F,G,H,I,J,K,L]
	}.

is_straight(st(As)) -->
	[A], [B], [C], [D], [E], [F], [G], [H], [I], [J], [K],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4), next_rank(R4, R5)
	, next_rank(R5, R6), next_rank(R6, R7), next_rank(R7, R8), next_rank(R8, R9)
	, next_rank(R9, R10), next_rank(R10, R11)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, card((R5, _)) = card(E)
	, card((R6, _)) = card(F)
	, card((R7, _)) = card(G)
	, card((R8, _)) = card(H)
	, card((R9, _)) = card(I)
	, card((R10, _)) = card(J)
	, card((R11, _)) = card(K)
	, As = [A,B,C,D,E,F,G,H,I,J,K]
	}.

is_straight(st(As)) -->
	[A], [B], [C], [D], [E], [F], [G], [H], [I], [J],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4), next_rank(R4, R5)
	, next_rank(R5, R6), next_rank(R6, R7), next_rank(R7, R8), next_rank(R8, R9)
	, next_rank(R9, R10)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, card((R5, _)) = card(E)
	, card((R6, _)) = card(F)
	, card((R7, _)) = card(G)
	, card((R8, _)) = card(H)
	, card((R9, _)) = card(I)
	, card((R10, _)) = card(J)
	, As = [A,B,C,D,E,F,G,H,I,J]
	}.

is_straight(st(As)) -->
	[A], [B], [C], [D], [E], [F], [G], [H], [I],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4), next_rank(R4, R5)
	, next_rank(R5, R6), next_rank(R6, R7), next_rank(R7, R8), next_rank(R8, R9)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, card((R5, _)) = card(E)
	, card((R6, _)) = card(F)
	, card((R7, _)) = card(G)
	, card((R8, _)) = card(H)
	, card((R9, _)) = card(I)
	, As = [A,B,C,D,E,F,G,H,I]
	}.

is_straight(st(As)) -->
	[A], [B], [C], [D], [E], [F], [G], [H],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4), next_rank(R4, R5)
	, next_rank(R5, R6), next_rank(R6, R7), next_rank(R7, R8)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, card((R5, _)) = card(E)
	, card((R6, _)) = card(F)
	, card((R7, _)) = card(G)
	, card((R8, _)) = card(H)
	, As = [A,B,C,D,E,F,G,H]
	}.

is_straight(st(As)) -->
	[A], [B], [C], [D], [E], [F], [G],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4), next_rank(R4, R5)
	, next_rank(R5, R6), next_rank(R6, R7)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, card((R5, _)) = card(E)
	, card((R6, _)) = card(F)
	, card((R7, _)) = card(G)
	, As = [A,B,C,D,E,F,G]
	}.

is_straight(st(As)) -->
	[A], [B], [C], [D], [E], [F],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4), next_rank(R4, R5)
	, next_rank(R5, R6)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, card((R5, _)) = card(E)
	, card((R6, _)) = card(F)
	, As = [A,B,C,D,E,F]
	}.

is_straight(st(As)) -->
	[A], [B], [C], [D], [E],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4), next_rank(R4, R5)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, card((R5, _)) = card(E)
	, As = [A,B,C,D,E]
	}.

is_straight(st(As)) -->
	[A], [B], [C], [D],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, As = [A,B,C,D]
	}.

is_straight(st(As)) -->
	[A], [B], [C],
	{ card((R1, _)) = card(A), next_rank(R1, R2), next_rank(R2, R3)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, As = [A,B,C]
	}.

hand([]) --> [].
hand([What | Ls]) -->
	( is_bomb(What)
	; is_chop(What)
	; is_straight(What)
	; is_triple(What)
	; is_double(What)
	; is_single(What)
	)
	, hand(Ls).

shuffe(Bag, Shuffled) :-
	findall(N, (member(M, Bag), nth0(N, Bag, M)), Indices),
	lists:perm(Indices, Ls),
	findall(M, (member(I, Ls), nth0(I, Bag, M)), Shuffled).

optimize(Bag, Solution) :-
	findnsols(1, H, order_by([desc(Value)]
		, 	( Bag = [Head | Rest]
			, shuffe(Rest, Shuffled)
			, hand(H, [Head | Shuffled], [])
			, value(H, Value)
			)
		), Solutions),
	nth0(0, Solutions, Solution).

concurrent_optimize(Bag, Solution) :-
	findall([Head | Rest], select(Head, Bag, Rest), Variants),
	format('Variants: ~w ~n', [Variants]),nl,
	concurrent_maplist(optimize, Variants, Intermediates),
	%% Merge and elect the best solution
	maplist(value, Intermediates, Values),
	max_member(Max, Values),
	nth0(Index, Values, Max),
	nth0(Index, Intermediates, Solution).


main :-
	Bag =
		[ (ace, heart)
		, (ace, spade)
		, (pig, spade)
		, (pig, heart)
		, (queen, spade)
		, (jack, spade)
		, (king, spade)
		%% , (six, spade)
		%% , (seven, spade)
		%% , (eight, spade)
		%% , (ten, spade)
		%% , (ten, spade)
		%% , (ace, spade)
		],

 	concurrent_optimize(Bag, H),
	format('Best solution: ~w~n', [H]).
