add(X,0,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).

multiply(X,0,0).
multiply(X,s(Y),Z) :- multiply(X,Y,Z'), add(Z',X,Z).

power(X, 0, 1).
power(X, s(Y), Z) :- power(X, Y, Z'), multiply(X, Z', Z).