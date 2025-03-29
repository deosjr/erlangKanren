-module(arithmetic).
-include("macros.erl").
-import(actors, [run/1, run/2, disj/2, disj_plus/1, equalo/2, call_fresh/1, conj_plus/1, delay/1, disj_conc/1]).

build_num(0) -> [];
build_num(N) when N rem 2 == 0 ->
    [0|build_num(N div 2)];
build_num(N) when N rem 2 == 1 ->
    [1|build_num((N-1) div 2)].

zeroO(N) -> equalo([], N).

posO(N) -> ?fresh(A,D, [equalo([A|D], N)]).

gt1O(N) -> ?fresh(A, AD, DD, [equalo([A, AD|DD], N)]).

fullAdderO(B, X, Y, R, C) ->
    ?conde(
        [equalo(0, B), equalo(0, X), equalo(0, Y), equalo(0, R), equalo(0, C)],
        [equalo(1, B), equalo(0, X), equalo(0, Y), equalo(1, R), equalo(0, C)],
        [equalo(0, B), equalo(1, X), equalo(0, Y), equalo(1, R), equalo(0, C)],
        [equalo(1, B), equalo(1, X), equalo(0, Y), equalo(0, R), equalo(1, C)],
        [equalo(0, B), equalo(0, X), equalo(1, Y), equalo(1, R), equalo(0, C)],
        [equalo(1, B), equalo(0, X), equalo(1, Y), equalo(0, R), equalo(1, C)],
        [equalo(0, B), equalo(1, X), equalo(1, Y), equalo(0, R), equalo(1, C)],
        [equalo(1, B), equalo(1, X), equalo(1, Y), equalo(1, R), equalo(1, C)]
    ).

adderO(D, N, M, R) ->
    ?delay(?conde(
        [equalo(0, D), equalo([], M), equalo(N, R)],
        [equalo(0, D), equalo([], N), equalo(M, R), posO(M)],
        [equalo(1, D), equalo([], M), adderO(0, N, [1], R)],
        [equalo(1, D), equalo([], N), posO(M), adderO(0, [1], M, R)],
        [equalo([1], N), equalo([1], M), ?fresh(A,C, [equalo([A,C], R), fullAdderO(D, 1, 1, A, C)])],
        [equalo([1], N), genAdderO(D, N, M, R)],
        [equalo([1], M), gt1O(N), gt1O(R), adderO(D, [1], N, R)],
        [gt1O(N), genAdderO(D, N, M, R)]
    )).

genAdderO(D, N, M, R) ->
    ?fresh(A,B,C,E,X,Y,Z, [
        equalo([A|X], N),
        equalo([B|Y], M), posO(Y),
        equalo([C|Z], R), posO(Z),
        fullAdderO(D, A, B, C, E),
        adderO(E, X, Y, Z)
    ]).

plusO(N, M, K) -> adderO(0, N, M, K).
minusO(N, M, K) -> plusO(M, K, N).

main(_) ->
    avl:init(),
    Out = run([?fresh(Q,X,Y, [equalo(Q, [X,Y]), plusO(X, Y, build_num(1000))])]),
    io:format("~w\n", [length(Out)]).   % currently takes ~2.2sec
