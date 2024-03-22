blau(sonnalm).
blau(vorkogel).
blau(arbiskogel).
blau(plattenalm).
blau(wiesenalm).
rot(isskogel).
schwarz(teufeltal).

start(sonnalm).
start(teufeltal).

endetIn(sonnalm, vorkogel).
endetIn(sonnalm, arbiskogel).
endetIn(vorkogel, isskogel).
endetIn(isskogel, tal).
endetIn(arbiskogel, plattenalm).
endetIn(plattenalm, wiesenalm).
endetIn(teufeltal, wiesenalm).
endetIn(wiesenalm, tal).

pathOfLength(cons(tal, nil), 0).
pathOfLength(cons(A, cons(B, C)), s(N)) :- endetIn(A, B), pathOfLength(cons(B, C), N).

add(X, 0, X).
add(X, s(Y), s(Z)) :- add(X, Y, Z).

connect(nil, X, X).
connect(cons(X, Y), Z, cons(X, Res)) :- connect(Y, Z, Res).

tourOfLength(cons(tal, nil), 0).
tourOfLength(A, N) :- add(T, P, N), 
                      start(Start),          
                      pathOfLength(cons(Start, A1), P),
                      connect(cons(tal, cons(Start, A1)), A2, A),
                      tourOfLength(cons(tal, A2), T).
                    
partTour(P, T) :- connect(_, P, P2), connect(P2, _, T), tourOfLength(P, _), tourOfLength(T, _).

%convert(nil, []).
%convert(cons(A, B), [A | C]) :- convert(B, C).

enumerateTours(S, L) :- tourOfLength(S, L).
enumerateTours(T, L) :- enumerateTours(T, s(L)).
enumerateTours(T) :- enumerateTours(T, 0).

/*
rotSchwarz([], 0, 0).
rotSchwarz([A | B], s(R), S) :- rot(A), rotSchwarz(B, R, S).
rotSchwarz([A | B], R, s(S)) :- schwarz(A), rotSchwarz(B, R, S).
rotSchwarz([A | B], R, S) :- blau(A), rotSchwarz(B, R, S).
rotSchwarz([tal | B], R, S) :- rotSchwarz(B, R, S).

tourRotSchwarz(T, R, S) :- enumerateTours(T), rotSchwarz(T, R, S). 
*/
