equal(replace(variable(X), V, T), R)	:-	X = V, equal(T, R).
equal(replace(apply(X, Y), V, T), apply(replace(X, V, T), replace(Y, V, Y))).

equal(apply(not, true), false).
equal(apply(not, false), true).

equal(apply(apply(equal, X), Y), true)		:-	equal(X, Y).

equal(A, B)								:-	A = B.
equal(A, B)								:-	equal(B, A).
equal(apply(A, B), apply(C, D))	:-	equal(A, C), equal(B, D).