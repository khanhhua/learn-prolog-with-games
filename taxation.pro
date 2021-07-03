%% Facts and relationships
category(milk, food).
category(cookies, food).
category(wine, luxury).

tax_rate(food, 5).
tax_rate(luxury, 60).

product(milk, 3).
product(wine, 5).
product(cookies, 1.4).

%% Queries and constraints
what_tax(X, Z) :-
    product(X),
    category(X, Y),
    tax_rate(Y, Z).

sell_price(X, Price) :-
    product(X, Cost),
    category(X, Category),
    tax_rate(Category, Tax),
    Price is Cost + Tax / 100.0.

sales_strategy(What, Min_Price) :- 
    sell_price(What, Price),
    Price >= Min_Price.

:-
    write('Enter your Objective Min Price: '),
    read(Min_Price),
    findall(What, sales_strategy(What, Min_Price), Solutions),
    ( Solutions == []
    -> write('No solutions')
    ; format('Solutions: ~w', [Solutions])
    ).

%% Expected console output
%% 
%% ?- ['taxation.pro'].
%% Enter your Objective Min Price: 1
%% |: .
%% Solutions: [milk,wine,cookies]
%% 
%% ?- ['taxation.pro'].
%% Enter your Objective Min Price: 2.
%% Solutions: [milk,wine]
%% true.
