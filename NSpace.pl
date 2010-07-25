unique(true).
unique(false).
unique(not).
unique(equal).


equal(replace(variable(X), V, T), R)	:-	X = V, equal(T, R).
equal(replace(const(X), _, _), R)		:-	equal(const(X), R).
equal(replace(lambda(LV, X), V, _), R)	:-	LV = V, equal(lambda(LV, X), R).
equal(replace(lambda(LV, X), V, T), R)	:-	LV \= V, equal(lambda(replace(X, V, T), LV), R).
equal(replace(apply(X, Y), V, T), apply(replace(X, V, T), replace(Y, V, Y))).

equal(apply(const(not), const(true)), const(false)).
equal(apply(const(not), const(false)), const(true)).

equal(apply(apply(const(equal), X), Y), true)		:-	equal(X, Y).

unequal(A, B)							:-	unique(A), unique(B), A \= B.

equal(A, B)								:-	A = B.
equal(A, B)								:-	equal(B, A).
equal(apply(A, B), apply(C, D))	:-	equal(A, C), equal(B, D).