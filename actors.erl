-module(actors).
-export([start/0]).

var(C) -> {var, C}.

empty_state() -> {#{}, 0}.
call_empty_state(G) -> G(empty_state()).

%% stream = actor
%% request more by sending {req, ReqPid}
%% receive answers as {more, {Sub,VC}}, {nomore, {Sub,VC} or nomore
%% NOTE: currently work only starts upon request
%% TODO: main should spawn a process that takes queries
%% and returns answers using take or take_all

%% TODO: this is bounded, so again could ignore the request for more
%% and just send the results immediately
equalo(U, V) ->
    fun({Sub,VC}) ->
        spawn(fun() ->
            receive
                {req, ReqPid} ->
                    case unify(U, V, Sub) of
                        false -> ReqPid ! nomore;
                        S -> ReqPid ! {nomore, {S,VC}}
                    end
            end
        end)
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

disj(G1, G2) -> fun(State) -> spawn(fun() -> mplus(G1(State), G2(State)) end) end.

conj(G1, G2) -> fun(State) -> spawn(fun() -> bind(G1(State), G2) end) end.

mplus(Str1, Str2) ->
    receive
        {req, ReqPid} ->
            Str1 ! {req, self()},
            receive
                nomore ->
                    Str2 ! {req, self()},
                    forward(ReqPid, Str2);
                {nomore, State} ->
                    ReqPid ! {more, State},
                    forward(ReqPid, Str2);
                {more, State} ->
                    ReqPid ! {more, State},
                    mplus(Str2, Str1)
                %F when is_function(F) ->
                    %% NOTE: sending func upstream again :(
                    %ReqPid ! fun() -> mplus(Str2, Str1()) end;
                %% as opposed to another more req
            end;
        endofreq -> exit("endofreq") % todo either spawn_link still or propagate endofreq to children
    end.

bind(Stream, G) ->
    receive
        {req, ReqPid} ->
            Stream ! {req, self()},
            receive
                nomore ->
                    ReqPid ! nomore;
                {nomore, State} ->
                    self() ! {req, ReqPid},
                    New = G(State),
                    forward(ReqPid, New);
                {more, State} ->
                    self() ! {req, ReqPid},
                    Bind = spawn(fun() -> bind(Stream, G) end),
                    mplus(G(State), Bind)
                %F when is_function(F) ->
                    %% NOTE: sending func upstream again :(
                    %ReqPid ! fun() -> bind(Stream(), G) end;
                %% as opposed to another more req
            end;
        endofreq -> exit("endofreq")
    end.

forward(Req, Stream) ->
    receive 
        {req, Req} -> Stream ! {req, self()};
        endofreq -> exit("endofreq");
        X -> Req ! X
    end,
    forward(Req, Stream).

-define(zzz(G), fun(State) -> fun() -> apply(G, [State]) end end).

disj_plus([G]) -> ?zzz(G);
disj_plus([G|T]) -> disj(?zzz(G), disj_plus(T)).

conj_plus([G]) -> ?zzz(G);
conj_plus([G|T]) -> conj(?zzz(G), conj_plus(T)).

-define(fresh(Goals), conj_plus(Goals)).
-define(fresh(X, Goals), call_fresh(fun(X) -> conj_plus(Goals) end)).
-define(fresh(X,Y, Goals), call_fresh(fun(X) -> call_fresh(fun(Y) -> conj_plus(Goals) end) end)).
%% etc..

take_all(S) ->
    S ! {req, self()},
    receive
        nomore -> [];
        {nomore, State} -> [State];
        {more, State} -> [State|take_all(S)]
    end.

take(N, S) when N > 0 ->
    S ! {req, self()},
    receive
        nomore -> [];
        {nomore, State} -> [State];
        {more, State} -> M=N-1, [State|take(M, S)]
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

main(_) ->
    %Out = run(9, [?fresh(X, [disj_plus([fives(X), sixes(X), sevens(X)])])]),
    %Out = mK_reify(take_all(call_empty_state(call_fresh(fun(X) -> equalo(X, 3) end)))),
    %Out = mK_reify(take_all(call_empty_state(call_fresh(fun(X) -> disj(equalo(X, 5), equalo(X, 6)) end)))),
    %Out = mK_reify(take(1, call_empty_state(call_fresh(fun(X) -> disj(equalo(X, 5), equalo(X, 6)) end)))),
    Out = mK_reify(take_all(call_empty_state(call_fresh(fun(X) -> call_fresh(fun(Y) -> conj(equalo(X, 5), equalo(Y, 6)) end) end)))),
    io:format("~w\n", [Out]).

start() ->
    Out = mK_reify(take_all(call_empty_state(call_fresh(fun(X) -> equalo(X, 3) end)))),
    io:format("~w\n", [Out]).
