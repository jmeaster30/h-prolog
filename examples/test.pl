siblings(X, Y) :- parent(Z, X), parent(Z, Y).

parent('alice', 'zoey').
parent('alice', 'cass').
parent('alice', 'luna').