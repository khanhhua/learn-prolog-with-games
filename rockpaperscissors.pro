hand(rock).
hand(paper).
hand(scissors).

wins(rock, scissors).
wins(paper, rock).
wins(scissors, paper).

loses(X, Y) :- wins(Y, X).
