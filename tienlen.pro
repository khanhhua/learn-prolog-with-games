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

bomb(A, B, C, D) :- maplist(card, [A,B,C,D]).
chop(A, B, C, D, E, F) --> maplist(card, [A,B,C,D,E,F]).
triple(A, B, C) :- maplist(card, [A,B,C]).
pair(A, B) :- maplist(card, [A,B]).
single(A) :- card(A).
/**
 * Combination definitions in DCG 
 */
is_bomb -->
	[A], [B], [C], [D],
	{ card((R, S1)) = card(A)
	, card((R, S2)) = card(B)
	, card((R, S3)) = card(C)
	, card((R, S4)) = card(D)
	, !
	, distinct(Member, member(Member, [S1, S2, S3, S4]))
	}.

is_chop -->
	[A], [B], [C], [D], [E], [F],
	{ card((R1, _)) = card(A), next_rank(R1, R2), next_rank(R2, R3)
	, card((R1, _)) = card(B)
	, card((R2, _)) = card(C)
	, card((R2, _)) = card(D)
	, card((R3, _)) = card(E)
	, card((R3, _)) = card(F)
	}.

is_straight, [B, C]-->
	[A], [B], [C],
	{ card((R1, _)) = card(A), next_rank(R1, R2), next_rank(R2, R3)
	, card((R2, _)) = card(B)
	, card((R3, _)) = card(C)
	}.

is_triple -->
	[A], [B], [C],
	{ card((R1, _)) = card(A)
	, card((R1, _)) = card(B)
	, card((R1, _)) = card(C)
	}.

is_pair -->
	[A], [B],
	{ card((R1, _)) = card(A)
	, card((R1, _)) = card(B)
	}.

is_single -->
	[A],
	{ card(A) }.

hand --> [].
hand -->
	( is_bomb
	; is_chop
	; is_straight
	; is_triple
	; is_pair
	; is_single
	),
	hand.

main :-
	Ls = [ (ace,heart)
		 , (ace,diamond)
		 , (ace,club)
		 , (X,spade)
		 , (king, heart)
		 , (king, diamond)
		 , (king, club)
		 , (queen, diamond)
		 , (queen, club)
		 , (Y, spade)
		 , (four, spade)
		 , (five, spade)
		 , (six, spade)
		 , (Z, spade)
		 ],
	phrase(hand, Ls),
	write([X,Y,Z]).
