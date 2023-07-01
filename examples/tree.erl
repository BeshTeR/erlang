%%% ----------------------------------------------------------------------------
%%% @doc Бинарное дерево
%%% @end
%%% ----------------------------------------------------------------------------

-module(tree).
-export([empty/0, insert/3, lookup/2, has_value/2]).

%% Пустое дерево
empty() -> 
    {node, nil}.

insert(Key, Val, {node, nil}) -> 
    {node, Key, Val, empty(), empty()};
insert(Key, Val, {node, Key, _, L, R}) ->
    {node, Key, Val, L, R};
insert(Key, Val, {node, K, V, L, R}) when Key < K ->
    {node, K, V, insert(Key, Val, L), R};
insert(Key, Val, {node, K, V, L, R}) when Key > K ->
    {node, K, V, L, insert(Key, Val, R)}.

%% Поиск по ключу
lookup(_, {node, nil}) ->
    undefined;
lookup(Key, {node, Key, Val, _, _}) ->
    {ok, Val};
lookup(Key, {node, K, _, L, _}) when Key < K ->
    lookup(Key, L);
lookup(Key, {node, _, _, _, R}) ->
    lookup(Key, R).

%% Значение содержится в дереве?
has_value(Val, Tree) ->
    try has_value({Val, Tree}) of
        false -> false
    catch
        true -> true
    end.
has_value({_, {node, nil}}) ->
    false;
has_value({Val, {node, _, Val, _, _}}) ->
    throw(true);
has_value({Val, {node, _, _, L, R}}) ->
    has_value({Val, L}),
    has_value({Val, R}).