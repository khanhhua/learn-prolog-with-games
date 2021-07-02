hand(rock).
hand(paper).
hand(scissors).

wins(rock, scissors).
wins(paper, rock).
wins(scissors, paper).

draws(X, X) :- true.
loses(X, Y) :- wins(Y, X), not(draws(X, Y)).