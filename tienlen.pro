:- use_module(library(clpfd)).
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
is_flush(fl(As)) -->
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

is_flush(fl(As)) -->
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

is_flush(fl(As)) -->
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

is_flush(fl(As)) -->
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

is_flush(fl(As)) -->
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

is_flush(fl(As)) -->
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

is_flush(fl(As)) -->
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

is_flush(fl(As)) -->
	[A], [B], [C], [D], [E],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4), next_rank(R4, R5)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, card((R5, _)) = card(E)
	, As = [A,B,C,D,E]
	}.

is_flush(fl(As)) -->
	[A], [B], [C], [D],
	{ card((R1, _)) = card(A)
	, next_rank(R1, R2), next_rank(R2, R3), next_rank(R3, R4)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	, card((R4, _)) = card(D)
	, As = [A,B,C,D]
	}.

is_flush(fl(As)) -->
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
	; is_flush(What)
	; is_triple(What)
	; is_double(What)
	; is_single(What)
	)
	, hand(Ls).

main :-
	Ls =[ (three, spade)
		, (four, spade)
		, (five, spade)
		, (six, spade)
		, (seven, spade)
		, (eight, spade)
		, (nine, spade)
		, (ten, spade)
		, (jack, spade)
		, (queen, spade)
		, (king, spade)
		, (ace, spade)
		, (ace, club)
		],
	findall(H, hand(H, Ls, []), Solutions),
	member(M, Solutions),
	format('Solution: ~w~n', [M]).
