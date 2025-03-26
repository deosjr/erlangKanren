-module(actors).
-include("macros.erl").
-export([start/0, call_fresh/1, run/1, run/2, equalo/2, disj/2, disj_plus/1, conj/2, conj_plus/1, delay/1]).

var(C) -> {var, C}.

empty_state() -> {#{}, 0}.
call_empty_state(G) -> G(empty_state()).

%% stream = actor
%% request more by sending {req, ReqPid}
%% receive answers as {more, {Sub,VC}}, {nomore, {Sub,VC}} or nomore
%% {forward, New} indicates stream is replaced by New stream
%% {forward, New, {Sub,VC}} combines answer and forward in one msg
%% NOTE: currently work only starts upon request
%% TODO: main should spawn a process that takes queries
%% and returns answers using take or take_all

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
        G = F(var(VC)),
        G({Sub,VC+1})
    end.

disj(G1, G2) -> fun(State) -> spawn(fun() -> mplus(G1(State), G2(State)) end) end.

conj(G1, G2) -> fun(State) -> spawn(fun() -> bind(G1(State), G2) end) end.

mplus(Str1, Str2) ->
    receive
        {req, ReqPid} ->
            Str1 ! {req, self()},
            receive
                nomore ->
                    ReqPid ! {forward, Str2};
                {nomore, State} ->
                    ReqPid ! {forward, Str2, State};
                {more, State} ->
                    ReqPid ! {more, State},
                    mplus(Str2, Str1);
                {forward, New} ->
                    self() ! {req, ReqPid},
                    mplus(New, Str2);
                {forward, New, State} ->
                    ReqPid ! {more, State},
                    mplus(Str2, New);
                delay ->
                    self() ! {req, ReqPid},
                    mplus(Str2, Str1)
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
                    New = G(State),
                    ReqPid ! {forward, New};
                {more, State} ->
                    self() ! {req, ReqPid},
                    Bind = spawn(fun() -> bind(Stream, G) end),
                    mplus(G(State), Bind);
                {forward, New} ->
                    self() ! {req, ReqPid},
                    bind(New, G);
                {forward, New, State} ->
                    self() ! {req, ReqPid},
                    Bind = spawn(fun() -> bind(New, G) end),
                    mplus(G(State), Bind);
                delay ->
                    self() ! {req, ReqPid},
                    bind(Stream, G)
            end;
        endofreq -> exit("endofreq")
    end.

delay(G) ->
    fun(State) ->
        spawn(fun() ->
            receive
                {req, ReqPid} -> ReqPid ! delay;
                endofreq -> exit("endofreq")
            end,
            receive
                % RPid needs a separate name otherwise compiler complains?
                {req, RPid} -> RPid ! {forward, G(State)};
                endofreq -> exit("endofreq")
            end
        end)
    end.

disj_plus([G]) -> G;
disj_plus([G|T]) -> disj(G, disj_plus(T)).

conj_plus([G]) -> G;
conj_plus([G|T]) -> conj(G, conj_plus(T)).

take_all(S) ->
    S ! {req, self()},
    receive
        nomore -> [];
        {nomore, State} -> [State];
        {more, State} -> [State|take_all(S)];
        {forward, Stream} -> take_all(Stream);
        {forward, Stream, State} -> [State|take_all(Stream)];
        delay -> take_all(S)
    end.

take(N, S) when N > 0 ->
    S ! {req, self()},
    receive
        nomore -> [];
        {nomore, State} -> [State];
        {more, State} -> [State|take(N-1, S)];
        {forward, Stream} -> take(N, Stream);
        {forward, Stream, State} -> [State|take(N-1, Stream)];
        delay -> take(N, S)
    end;
take(0, S) -> S ! endofreq, [].
    

walk_star(V, Sub) ->
    case walk(V, Sub) of
        {var, C} -> {var, C};
        [H|T] -> [walk_star(H, Sub)|walk_star(T, Sub)];
        X -> X
    end.

mK_reify(States) -> lists:map(fun({Sub,_}) -> walk_star(var(0), Sub) end, States).

run(N, Goals) -> mK_reify(take(N, call_empty_state(conj_plus(Goals)))).
run(Goals)   -> mK_reify(take_all(call_empty_state(conj_plus(Goals)))).

fives(X)  -> disj(equalo(X, 5), ?delay(fives(X))).
sixes(X)  -> disj(equalo(X, 6), ?delay(sixes(X))).
sevens(X) -> disj(equalo(X, 7), ?delay(sevens(X))).

main(_) ->
    Out = run(9, [?fresh(X, [disj_plus([fives(X), sixes(X), sevens(X)])])]),
    io:format("~w\n", [Out]).

start() ->
    Out = run(9, [?fresh(X, [disj_plus([fives(X), sixes(X), sevens(X)])])]),
    io:format("~w\n", [Out]).
