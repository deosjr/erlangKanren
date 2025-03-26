-define(delay(G), delay(fun(State) -> apply(G, [State]) end)).

-define(fresh(Goals), conj_plus(Goals)).
-define(fresh(X, Goals), call_fresh(fun(X) -> conj_plus(Goals) end)).
-define(fresh(X,Y, Goals), call_fresh(fun(X) ->
    call_fresh(fun(Y) -> conj_plus(Goals) end) end)).
-define(fresh(X,Y,Z, Goals), call_fresh(fun(X) ->
    call_fresh(fun(Y) -> call_fresh(fun(Z) -> conj_plus(Goals) end) end) end)).
-define(fresh(A,B,C,D, Goals), call_fresh(fun(A) ->
    call_fresh(fun(B) -> call_fresh(fun(C) -> call_fresh(fun(D) -> conj_plus(Goals) end) end) end) end)).
-define(fresh(A,B,C,D,E, Goals), call_fresh(fun(A) ->
    call_fresh(fun(B) -> call_fresh(fun(C) -> call_fresh(fun(D) ->
    call_fresh(fun(E) -> conj_plus(Goals) end) end) end) end) end)).
-define(fresh(A,B,C,D,E,F, Goals), call_fresh(fun(A) ->
    call_fresh(fun(B) -> call_fresh(fun(C) -> call_fresh(fun(D) ->
    call_fresh(fun(E) -> call_fresh(fun(F) -> conj_plus(Goals) end) end) end) end) end) end)).
-define(fresh(A,B,C,D,E,F,G, Goals), call_fresh(fun(A) ->
    call_fresh(fun(B) -> call_fresh(fun(C) -> call_fresh(fun(D) ->
    call_fresh(fun(E) -> call_fresh(fun(F) -> call_fresh(fun(G) -> conj_plus(Goals) end) end) end) end) end) end) end)).
%% etc..

-define(conde(Goals), conj_plus(Goals)).
-define(conde(G1, G2), disj_plus([conj_plus(G1), conj_plus(G2)])).
-define(conde(G1, G2, G3), disj_plus([conj_plus(G1), conj_plus(G2), conj_plus(G3)])).
-define(conde(G1, G2, G3, G4), disj_plus([
    conj_plus(G1), conj_plus(G2), conj_plus(G3), conj_plus(G4)])).
-define(conde(G1, G2, G3, G4, G5), disj_plus([
    conj_plus(G1), conj_plus(G2), conj_plus(G3), conj_plus(G4), conj_plus(G5)])).
-define(conde(G1, G2, G3, G4, G5, G6), disj_plus([
    conj_plus(G1), conj_plus(G2), conj_plus(G3), conj_plus(G4), conj_plus(G5), conj_plus(G6)])).
-define(conde(G1, G2, G3, G4, G5, G6, G7), disj_plus([
    conj_plus(G1), conj_plus(G2), conj_plus(G3), conj_plus(G4), conj_plus(G5), conj_plus(G6), conj_plus(G7)])).
-define(conde(G1, G2, G3, G4, G5, G6, G7, G8), disj_plus([
    conj_plus(G1), conj_plus(G2), conj_plus(G3), conj_plus(G4), conj_plus(G5), conj_plus(G6), conj_plus(G7), conj_plus(G8)])).
%% etc
