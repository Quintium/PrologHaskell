add(X, 0, X).
add(X, s(Y), s(Z)) :- add(X, Y, Z).

multiply(_, 0, 0).
multiply(X, s(Y), Z) :- multiply(X, Y, Z2), add(Z2, X, Z).

power(_, 0, s(0)).
power(X, s(Y), Z) :- power(X, Y, Z2), multiply(X, Z2, Z).