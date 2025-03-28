-module(avl).
-export([init/0, new/0, insert/3, lookup/2]).

%% immutable AVL tree with structure sharing, shared by reference
%% use ETS to store references to unique AVL tree nodes
%% references are generated using erlang:unique_integer
%% and default to 'undefined' when not set in record creation
%% does not support updates or deletions
%% TODO: I think there are lots of double lookups atm

-record(avl, {key, value, left, right, height}).

-define(TABLE, avl_tree).

init() -> ets:new(?TABLE, [set, named_table, public]).

new() -> undefined.

ref(undefined) -> undefined;
ref(Ref) -> [{Ref,AVL}] =  ets:lookup(?TABLE, Ref), AVL.

insert(Ref, Key, Value) ->
    case insert_ref(Ref, Key, Value) of
        found -> Ref;
        {new, NewRef} -> NewRef
    end.

insert_ref(undefined, Key, Value) -> {new,commit(#avl{key=Key, value=Value})};
insert_ref(Ref, Key, Value) -> insert_avl(ref(Ref), Key, Value).

insert_avl(#avl{key=Key}, Key, _) -> found;
insert_avl(AVL=#avl{key=K, left=Left}, Key, Value) when K > Key ->
    case insert_ref(Left, Key, Value) of
        found -> found;
        {new, Node} -> {new,rebalance(AVL#avl{left=Node})}
    end;
insert_avl(AVL=#avl{key=K, right=Right}, Key, Value) when K < Key ->
    case insert_ref(Right, Key, Value) of
        found -> found;
        {new, Node} -> {new,rebalance(AVL#avl{right=Node})}
    end.

rebalance(AVL=#avl{left=LRef, right=RRef}) ->
    Left = ref(LRef),
    Right = ref(RRef),
    Inbalance = height(Right) - height(Left),
    rebalance_(AVL, Left, Right, Inbalance).

rebalance_(AVL, Child, _, -2) ->
    Grandchild = ref(Child#avl.right),
    Other = ref(Child#avl.left),
    case height(Other) > height(Grandchild) of
        true -> commit(Child#avl{
            right=commit(AVL#avl{left=Child#avl.right})});
        false -> commit(Grandchild#avl{
            left=commit(Child#avl{right=Grandchild#avl.left}),
            right=commit(AVL#avl{left=Grandchild#avl.right})
        })
    end;
rebalance_(AVL, _, Child, 2) ->
    Grandchild = ref(Child#avl.left),
    Other = ref(Child#avl.right),
    case height(Other) > height(Grandchild) of
        true -> commit(Child#avl{
            left=commit(AVL#avl{right=Child#avl.left})});
        false -> commit(Grandchild#avl{
            left=commit(AVL#avl{right=Grandchild#avl.left}),
            right=commit(Child#avl{left=Grandchild#avl.right})
        })
    end;
rebalance_(AVL, _, _, _) -> commit(AVL).

height(undefined) -> 0;
height(#avl{height=H}) -> H.

commit(AVL=#avl{left=LRef, right=RRef}) ->
    Left = ref(LRef),
    Right = ref(RRef),
    N = AVL#avl{height=1+max(height(Left), height(Right))},
    Ref = erlang:unique_integer([]),
    ets:insert(?TABLE, {Ref, N}),
    Ref.

lookup(Ref, Key) -> lookup_(ref(Ref), Key).
lookup_(undefined, _) -> undefined;
lookup_(#avl{key=Key, value=Value}, Key) -> {ok, Value};
lookup_(#avl{key=K, left=L}, Key) when K > Key -> lookup(L, Key);
lookup_(#avl{key=K, right=R}, Key) when K < Key -> lookup(R, Key).
