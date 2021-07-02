product(milk).
product(wine).
product(cookies).

category(milk, food).
category(cookies, food).
category(wine, luxury).

tax_rate(food, 5).
tax_rate(luxury, 60).

what_tax(X, Z) :-
    product(X),
    category(X, Y),
    tax_rate(Y, Z).

