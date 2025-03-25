-module(kanren).
-export([start/0]).

var(C) -> {var, C}.

empty_state() -> {#{}, 0}.
call_empty_state(G) -> G(empty_state()).

equalo(U, V) ->
    fun({Sub,VC}) ->
        case unify(U, V, Sub) of
            false -> [];
            S -> [{S,VC}]
        end
    end.

unify(U, V, Sub) ->
    U0 = walk(U, Sub),
    V0 = walk(V, Sub),
    unify_(U0, V0, Sub).

%% todo: occurs check
unify_({var, C}, {var, C}, Sub) -> Sub;
unify_(U={var, _}, V, Sub) -> Sub#{U=>V};
unify_(U, V={var, _}, Sub) -> Sub#{V=>U};
unify_([UH|UT], [VH|VT], Sub) ->
    case unify(UH, VH, Sub) of
        false -> false;
        S -> unify(UT, VT, S)
    end;
unify_(X, X, Sub) -> Sub;
unify_(_, _, _) -> false.

    
walk(U={var,_}, Sub) ->
    case maps:find(U, Sub) of
        {ok,V} -> walk(V, Sub);
        _ -> U
    end;
walk(U, _) -> U.

call_fresh(F) ->
    fun({Sub,VC}) ->
        VC1 = VC + 1,
        G = F(var(VC)),
        G({Sub,VC1})
    end.

disj(G1, G2) -> fun(State) -> mplus(G1(State), G2(State)) end.

conj(G1, G2) -> fun(State) -> bind(G1(State), G2) end.

mplus(Str1, Str2) ->
    case Str1 of
        [] -> Str2;
        F when is_function(F) -> fun() -> mplus(Str2, Str1()) end;
        [H|T] -> [H|mplus(Str2, T)]
    end.

bind(Stream, G) ->
    case Stream of
        [] -> [];
        F when is_function(F) -> fun() -> bind(Stream(), G) end;
        [H|T] -> mplus(G(H), bind(T, G))
    end.

-define(zzz(G), fun(State) -> fun() -> apply(G, [State]) end end).

disj_plus([G]) -> ?zzz(G);
disj_plus([G|T]) -> disj(?zzz(G), disj_plus(T)).

conj_plus([G]) -> ?zzz(G);
conj_plus([G|T]) -> conj(?zzz(G), conj_plus(T)).

-define(fresh(Goals), conj_plus(Goals)).
-define(fresh(X, Goals), call_fresh(fun(X) -> conj_plus(Goals) end)).
-define(fresh(X,Y, Goals), call_fresh(fun(X) -> call_fresh(fun(Y) -> conj_plus(Goals) end) end)).
%% etc..

pull(F) when is_function(F) -> pull(F());
pull(S) -> S.

take_all(S) ->
    case pull(S) of
        [] -> [];
        [H|T] -> [H|take_all(T)]
    end.

take(N, S) when N > 0 ->
    case pull(S) of
        [] -> [];
        [H|T] -> M = N-1, [H|take(M, T)]
    end;
take(0, _) -> [].

walk_star(V, Sub) ->
    case walk(V, Sub) of
        {var, C} -> {var, C};
        [H|T] -> [walk_star(H, Sub)|walk_star(T, Sub)];
        X -> X
    end.

mK_reify(States) -> lists:map(fun({Sub,_}) -> walk_star(var(0), Sub) end, States).

run(N, Goals) -> mK_reify(take(N, call_empty_state(conj_plus(Goals)))).
run(Goals)   -> mK_reify(take_all(call_empty_state(conj_plus(Goals)))).

fives(X)  -> disj(equalo(X, 5), ?zzz( fives(X))).
sixes(X)  -> disj(equalo(X, 6), ?zzz( sixes(X))).
sevens(X) -> disj(equalo(X, 7), ?zzz(sevens(X))).

%% for use in scripting: escript kanren.erl
main(_) ->
    Out = run(9, [?fresh(X, [disj_plus([fives(X), sixes(X), sevens(X)])])]),
    io:format("~w\n", [Out]).

%% for use in init: erlc kanren.erl && erl -s kanren
start() -> X=var(42), io:format("~w\n", [X]).
