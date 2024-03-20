add(X,0,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).